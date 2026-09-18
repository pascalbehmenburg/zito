use crate::{
    IndexView, SearchOptions, SearchResult,
    index::Location,
    ngram,
    storage::{ArchivedDocument, ArchivedSegment},
};
use eyre::{Result, ensure};
use memchr::memmem::Finder;
use regex::Regex;
use regex_syntax::hir::literal::Extractor;

/// Work counters for comparing query policies without rerunning benchmarks.
#[derive(Clone, Copy, Debug, Default)]
pub struct SearchStats {
    pub candidate_documents: usize,
    pub verified_bytes: usize,
    pub posting_lookups: usize,
}

impl IndexView {
    pub fn search(
        &self,
        query: &str,
        options: SearchOptions,
    ) -> Result<Vec<SearchResult>> {
        Ok(self.search_with_stats(query, options)?.0)
    }

    /// Search the indexed snapshot. Literal matches overlap; regex matches use
    /// regex::Regex::find_iter semantics, independently on each logical line.
    /// Short literals and regexes without a usable literal scan live documents.
    pub fn search_with_stats(
        &self,
        query: &str,
        options: SearchOptions,
    ) -> Result<(Vec<SearchResult>, SearchStats)> {
        ensure!(
            options.regex || !query.is_empty(),
            "literal query must not be empty"
        );
        let regex = if options.regex {
            Some(Regex::new(query)?)
        } else {
            None
        };
        let keys = if options.regex {
            let hir = regex_syntax::parse(query)?;
            let mut literals = Extractor::new().extract(&hir);
            literals.dedup();
            match literals.literals() {
                Some(literals) if !literals.is_empty() => literals
                    .iter()
                    .map(|lit| ngram::query_keys(lit.as_bytes()))
                    .collect(),
                _ => vec![Vec::new()],
            }
        } else {
            vec![ngram::query_keys(query.as_bytes())]
        };
        let finder = Finder::new(query);
        let mut stats = SearchStats::default();
        let mut results = Vec::new();
        for (segment_id, segment) in self.segments.iter().enumerate() {
            let archive = segment.archive();
            let candidates = if keys.iter().any(Vec::is_empty) {
                (0..archive.entries.len() as u32).collect()
            } else {
                let mut candidates = Vec::new();
                for alternative in &keys {
                    candidates.extend(intersect(
                        archive,
                        alternative,
                        &mut stats,
                    ));
                }
                candidates.sort_unstable();
                candidates.dedup();
                candidates
            };
            for id in candidates {
                let entry = &archive.entries[id as usize];
                if self.live.get(entry.path.as_str())
                    != Some(&Location {
                        segment: segment_id,
                        entry: id as usize,
                    })
                {
                    continue;
                }
                let doc = entry.document.as_ref().unwrap();
                stats.candidate_documents += 1;
                stats.verified_bytes += doc.content.len();
                if let Some(regex) = &regex {
                    for (line_number, line) in doc.content.lines().enumerate() {
                        for found in regex.find_iter(line) {
                            results.push(SearchResult {
                                file_path: entry.path.to_string(),
                                line_number: line_number as u32,
                                line_text: line.into(),
                                match_start: found.start() as u32,
                                match_end: found.end() as u32,
                            });
                        }
                    }
                } else {
                    literal_matches(
                        entry.path.as_str(),
                        doc,
                        &finder,
                        query.len(),
                        &mut results,
                    );
                }
            }
        }
        results.sort_unstable_by(|a, b| {
            a.file_path
                .cmp(&b.file_path)
                .then(a.line_number.cmp(&b.line_number))
                .then(a.match_start.cmp(&b.match_start))
                .then(a.match_end.cmp(&b.match_end))
        });
        Ok((results, stats))
    }
}

fn intersect(
    segment: &ArchivedSegment,
    keys: &[u64],
    stats: &mut SearchStats,
) -> Vec<u32> {
    let mut lists = Vec::with_capacity(keys.len());
    for key in keys {
        stats.posting_lookups += 1;
        let Some(list) =
            segment.postings.get(&rkyv::rend::u64_le::from_native(*key))
        else {
            return Vec::new();
        };
        lists.push(list.as_slice());
    }
    lists.sort_unstable_by_key(|list| list.len());
    let Some(first) = lists.first() else {
        return Vec::new();
    };
    let mut candidates: Vec<_> =
        first.iter().map(|id| id.to_native()).collect();
    for list in lists.into_iter().skip(1) {
        if candidates.is_empty() {
            break;
        }
        if list.len() > candidates.len().saturating_mul(8) {
            candidates.retain(|id| {
                list.binary_search_by_key(id, |x| x.to_native()).is_ok()
            });
        } else {
            let mut cursor = 0;
            candidates.retain(|id| {
                while cursor < list.len() && list[cursor].to_native() < *id {
                    cursor += 1;
                }
                cursor < list.len() && list[cursor].to_native() == *id
            });
        }
    }
    candidates
}

fn literal_matches(
    path: &str,
    doc: &ArchivedDocument,
    finder: &Finder<'_>,
    query_len: usize,
    results: &mut Vec<SearchResult>,
) {
    let bytes = doc.content.as_bytes();
    let mut offset = 0;
    while let Some(relative) = finder.find(&bytes[offset..]) {
        let start = offset + relative;
        let end = start + query_len;
        let line = doc
            .line_starts
            .partition_point(|n| n.to_native() as usize <= start)
            - 1;
        let line_start = doc.line_starts[line].to_native() as usize;
        let mut line_end = doc
            .line_starts
            .get(line + 1)
            .map_or(bytes.len(), |n| n.to_native() as usize - 1);
        if line_end < bytes.len()
            && line_end > line_start
            && bytes[line_end - 1] == b'\r'
        {
            line_end -= 1;
        }
        if end <= line_end {
            results.push(SearchResult {
                file_path: path.into(),
                line_number: line as u32,
                line_text: doc.content[line_start..line_end].into(),
                match_start: (start - line_start) as u32,
                match_end: (end - line_start) as u32,
            });
        }
        // Byte searching permits overlapping matches without slicing a str at
        // a non-UTF-8 boundary. A valid UTF-8 needle matches at valid boundaries.
        offset = start + 1;
    }
}
