//! A fast text search engine using trigram indexing and Aho-Corasick pattern matching.
//!
//! This library provides efficient full-text search capabilities by building trigram-based
//! inverted indices. It uses the daachorse crate for fast pattern matching and rkyv for
//! efficient serialization of the index data.

use daachorse::DoubleArrayAhoCorasick;
use eyre::{Result, eyre};
use flate2::Compression;
use fxhash::FxHashMap;
use mimalloc::MiMalloc;
use rkyv::{
    Archive, Deserialize, Serialize, access_unchecked, api::serialize_using,
    ser::writer::IoWriter, util::with_arena,
};
use smallvec::SmallVec;
use std::{
    fs::File,
    io::{BufRead, Read},
    ops::Deref,
    path::Path,
};
use walkdir::WalkDir;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// A space-efficient identifier for interned strings.
///
/// String interning reduces memory usage by storing each unique string only once
/// and referencing it by a small integer ID.
#[derive(
    Archive, Deserialize, Serialize, Copy, Clone, Debug, PartialEq, Eq, Hash,
)]
#[rkyv(derive(Debug, Hash, PartialEq, Eq))]
pub struct InternedStringId(u32);

/// Manages string interning to reduce memory usage for file paths.
///
/// File paths are often repeated or share common prefixes, so interning
/// them significantly reduces the memory footprint of the index.
#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct StringInterner {
    /// Maps strings to their interned IDs for fast lookup.
    pub map: FxHashMap<String, InternedStringId>,
    /// Stores the actual string data indexed by ID.
    pub vec: Vec<String>,
}

impl StringInterner {
    /// Creates a new empty string interner.
    pub fn new() -> Self {
        Self {
            map: FxHashMap::default(),
            vec: Vec::new(),
        }
    }

    /// Interns a string and returns its ID.
    ///
    /// If the string has already been interned, returns the existing ID.
    /// Otherwise, assigns a new ID and stores the string.
    pub fn intern(&mut self, s: &str) -> InternedStringId {
        if let Some(&id) = self.map.get(s) {
            return id;
        }
        let id = InternedStringId(self.vec.len() as u32);
        self.vec.push(s.to_string());
        self.map.insert(s.to_string(), id);
        id
    }
}

impl ArchivedStringInterner {
    /// Resolves an interned string ID back to its original string.
    pub fn resolve(&self, id: ArchivedInternedStringId) -> &str {
        &self.vec[id.0.to_native() as usize]
    }
}

/// Unique identifier for a file in the index.
pub type FileId = u32;

/// Byte offset within a file or line.
pub type Offset = u32;

/// A three-byte sequence used for trigram indexing.
pub type Trigram = [u8; 3];

/// Represents a line within a file, storing its byte boundaries.
///
/// Lines are tracked to enable efficient line-based search results
/// without having to re-parse files during search operations.
#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct Line {
    /// Starting byte offset of the line within the file.
    pub start: Offset,
    /// Ending byte offset of the line within the file (exclusive).
    pub end: Offset,
}

/// A posting entry that records where a trigram appears in the corpus.
///
/// Each posting contains location information that allows the search engine
/// to quickly locate potential matches and verify them against the full query.
#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct Posting {
    /// Line number within the file where this trigram appears.
    pub line_number: u32,
    /// Byte offset within the file where this trigram ends.
    pub byte_offset: u32,
    /// Identifier of the file containing this trigram.
    pub file_id: InternedStringId,
}

/// Initial capacity for posting lists to optimize memory allocation.
const POSTING_CAPACITY: usize = 16;

/// A collection of postings for a specific trigram.
///
/// Uses SmallVec to optimize for the common case where trigrams appear
/// in only a few locations, avoiding heap allocation for small lists.
#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct PostingList {
    data: SmallVec<[Posting; POSTING_CAPACITY]>,
}

impl PostingList {
    /// Creates a new posting list with the specified capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: SmallVec::with_capacity(capacity),
        }
    }

    /// Adds a new posting to this list.
    #[inline]
    fn push(&mut self, posting: Posting) {
        self.data.push(posting);
    }

    /// Returns the posting at the specified index, if it exists.
    #[inline]
    pub fn get(&self, index: usize) -> Option<&Posting> {
        self.data.get(index)
    }

    /// Returns all postings as a slice.
    #[inline]
    pub fn as_slice(&self) -> &[Posting] {
        self.data.as_slice()
    }
}

impl<'a> IntoIterator for &'a PostingList {
    type Item = &'a Posting;
    type IntoIter = std::slice::Iter<'a, Posting>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

impl<'a> IntoIterator for &'a ArchivedPostingList {
    type Item = &'a ArchivedPosting;
    type IntoIter = std::slice::Iter<'a, ArchivedPosting>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

/// The main search index containing all indexed data.
///
/// This structure holds the trigram inverted index, file path mappings,
/// line boundary information, and file contents needed for fast text search.
#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct Index {
    /// Interner for file paths to reduce memory usage.
    pub interned_paths: StringInterner,
    /// Maps file IDs to their line boundary information.
    pub lines: FxHashMap<InternedStringId, Vec<Line>>,
    /// Maps file IDs to their actual content.
    pub file_contents: FxHashMap<InternedStringId, Vec<u8>>,
    /// Inverted index mapping trigrams to their posting lists.
    pub trigrams: FxHashMap<Trigram, PostingList>,
}

/// Represents a single search result with location and context information.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SearchResult {
    /// Path to the file containing the match.
    pub file_path: String,
    /// Line number within the file (0-indexed).
    pub line_number: u32,
    /// Complete text of the line containing the match.
    pub line_text: String,
    /// Character offset where the match starts within the line.
    pub match_start: u32,
    /// Character offset where the match ends within the line.
    pub match_end: u32,
}

/// A read-only view of a serialized index for performing searches.
///
/// This structure provides access to a compressed, serialized index
/// without requiring deserialization of the entire data structure.
pub struct IndexView {
    buf: Vec<u8>,
}

impl IndexView {
    /// Searches the index for the given query string.
    ///
    /// Returns all matches found in the indexed files, with each result
    /// containing the file path, line number, line text, and match position.
    ///
    /// # Arguments
    ///
    /// * `query` - The text to search for (must be at least 3 characters)
    ///
    /// # Returns
    ///
    /// A vector of search results, sorted by file path and line number.
    ///
    /// # Errors
    ///
    /// Returns an error if the Aho-Corasick automaton cannot be built
    /// or if there are issues reading the indexed files.
    pub fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
        if query.len() < 3 {
            return Ok(Vec::new());
        }

        let trigram_index = self.trigrams.iter();

        let daac = DoubleArrayAhoCorasick::with_values(trigram_index).map_err(
            |e| eyre::eyre!("Failed to build Aho-Corasick automaton: {}", e),
        )?;

        let mut results = Vec::new();

        let mut candidate_postings = Vec::new();
        for m in daac.find_iter(query.as_bytes()) {
            let posting_list = m.value();
            for posting in posting_list {
                candidate_postings.push(posting);
            }
        }

        if candidate_postings.is_empty() {
            return Ok(Vec::new());
        }

        let mut grouped_by_file_line: FxHashMap<
            (u32, u32),
            Vec<&ArchivedPosting>,
        > = FxHashMap::default();

        for posting in candidate_postings {
            let file_id = posting.file_id.0.to_native();
            let line_number = posting.line_number.to_native();
            grouped_by_file_line
                .entry((file_id, line_number))
                .or_insert_with(Vec::new)
                .push(posting);
        }

        // Cache file contents to avoid processing the same file multiple times
        let mut file_cache: FxHashMap<String, String> = FxHashMap::default();

        for ((file_id, line_number), _postings) in grouped_by_file_line {
            let file_id_interned =
                ArchivedInternedStringId(rkyv::Archived::<u32>::from_native(
                    file_id,
                ));
            let file_path =
                self.interned_paths.resolve(file_id_interned).to_string();

            let file_content = if let Some(content) = file_cache.get(&file_path)
            {
                content.clone()
            } else {
                // Get content from the stored file contents in the index
                let file_id_for_content = ArchivedInternedStringId(
                    rkyv::Archived::<u32>::from_native(file_id),
                );
                if let Some(stored_content) =
                    self.file_contents.get(&file_id_for_content)
                {
                    match String::from_utf8(stored_content.to_vec()) {
                        Ok(content) => {
                            file_cache
                                .insert(file_path.clone(), content.clone());
                            content
                        }
                        Err(_) => continue, // Skip files with invalid UTF-8
                    }
                } else {
                    continue; // Skip if content not found in index
                }
            };

            let lines: Vec<&str> = file_content.lines().collect();

            if line_number as usize >= lines.len() {
                continue;
            }

            let line_text = lines[line_number as usize];

            let mut start_pos = 0;
            while let Some(match_pos) = line_text[start_pos..].find(query) {
                let absolute_match_pos = start_pos + match_pos;

                let search_result = SearchResult {
                    file_path: file_path.clone(),
                    line_number,
                    line_text: line_text.to_string(),
                    match_start: absolute_match_pos as u32,
                    match_end: (absolute_match_pos + query.len()) as u32,
                };
                results.push(search_result);

                start_pos = absolute_match_pos + 1;
            }
        }

        results.sort_by(|a, b| {
            a.file_path
                .cmp(&b.file_path)
                .then(a.line_number.cmp(&b.line_number))
                .then(a.match_start.cmp(&b.match_start))
        });
        results.dedup_by(|a, b| {
            a.file_path == b.file_path
                && a.line_number == b.line_number
                && a.match_start == b.match_start
        });

        Ok(results)
    }
}

impl Deref for IndexView {
    type Target = ArchivedIndex;

    fn deref(&self) -> &Self::Target {
        unsafe { access_unchecked::<ArchivedIndex>(&self.buf) }
    }
}

impl TryFrom<&Path> for IndexView {
    type Error = eyre::Error;

    /// Creates an IndexView by loading and decompressing an index file.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the compressed index file
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be opened or decompressed.
    fn try_from(path: &Path) -> Result<Self> {
        let mut decompressor =
            flate2::read::ZlibDecoder::new(File::open(path)?);
        let mut buf = Vec::new();
        decompressor.read_to_end(&mut buf)?;
        Ok(IndexView { buf })
    }
}

impl Index {
    /// Creates a new empty index.
    pub fn new() -> Self {
        Index {
            interned_paths: StringInterner::new(),
            lines: FxHashMap::default(),
            file_contents: FxHashMap::default(),
            trigrams: FxHashMap::default(),
        }
    }

    /// Serializes and compresses the index to a file.
    ///
    /// The index is compressed using zlib compression to reduce storage size.
    ///
    /// # Arguments
    ///
    /// * `path` - Path where the index file should be written
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be created or if serialization fails.
    pub fn store<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let mut compressor = flate2::write::ZlibEncoder::new(
            File::create(path)?,
            Compression::best(),
        );
        with_arena(|arena| {
            let mut serializer = rkyv::ser::Serializer::new(
                IoWriter::new(&mut compressor),
                arena.acquire(),
                rkyv::ser::sharing::Share::new(),
            );
            serialize_using::<_, rkyv::rancor::Error>(self, &mut serializer)
        })?;
        Ok(())
    }

    /// Creates a new index by recursively scanning a directory.
    ///
    /// Walks through all files in the given directory and its subdirectories,
    /// indexing each file's content using trigram analysis.
    ///
    /// # Arguments
    ///
    /// * `path` - Root directory to scan for files
    ///
    /// # Returns
    ///
    /// A new Index containing trigram data for all readable files found.
    ///
    /// # Errors
    ///
    /// Returns an error if the directory cannot be accessed. Individual files
    /// that cannot be read or are too short to index are silently skipped.
    pub fn new_from_path<P: AsRef<Path>>(path: P) -> Result<Index> {
        let mut main_index = Index::new();
        for entry in WalkDir::new(path)
            .follow_links(false)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
        {
            if let Ok(file) = std::fs::File::open(entry.path()) {
                let reader = std::io::BufReader::new(file);
                let file_id = main_index.interned_paths.intern(
                    &entry.path().to_path_buf().to_string_lossy().to_string(),
                );
                if let Err(_) =
                    Self::add_file_to_index(&mut main_index, reader, file_id)
                {
                    continue;
                }
            }
        }
        Ok(main_index)
    }

    /// Adds a single file's content to the index.
    ///
    /// Reads the file content, extracts all trigrams, and builds posting lists
    /// that map each trigram to its locations within the file.
    ///
    /// # Arguments
    ///
    /// * `index` - The index to add the file to
    /// * `reader` - A buffered reader for the file content
    /// * `file_id` - The interned ID for this file's path
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or if it contains fewer
    /// than 3 bytes (minimum required for trigram extraction).
    fn add_file_to_index<R: BufRead>(
        index: &mut Index,
        mut reader: R,
        file_id: InternedStringId,
    ) -> Result<()> {
        let mut content =
            Vec::with_capacity(reader.fill_buf().map_or(0, |b| b.len()));
        reader.read_to_end(&mut content)?;

        if content.len() < 3 {
            return Err(eyre!("File too short to index"));
        }

        let mut line_number = 0;
        let mut line_start = 0;

        let window = content.as_ptr();
        let end = unsafe { window.add(content.len()) };
        let mut current = window;
        while unsafe { current.add(2) } < end {
            let c = unsafe { *current };

            if c == b'\n' {
                index.lines.entry(file_id).or_default().push(Line {
                    start: line_start,
                    end: (current as usize - window as usize) as Offset,
                });
                line_number += 1;
                line_start = (current as usize - window as usize + 1) as Offset;
            }

            let trigram =
                unsafe { [*current, *current.add(1), *current.add(2)] };

            index
                .trigrams
                .entry(trigram)
                .or_insert_with(|| PostingList::with_capacity(POSTING_CAPACITY))
                .push(Posting {
                    line_number,
                    byte_offset: unsafe {
                        current.add(2) as usize - window as usize
                    } as Offset,
                    file_id: file_id,
                });

            current = unsafe { current.add(1) };
        }

        // Handle the last line if it doesn't end with a newline
        let final_offset =
            unsafe { current.add(2).min(end) as usize - window as usize };
        if line_start as usize != final_offset {
            index.lines.entry(file_id).or_default().push(Line {
                start: line_start,
                end: final_offset as Offset,
            });
        }

        // Store the file content in the index
        index.file_contents.insert(file_id, content);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use daachorse::DoubleArrayAhoCorasick;
    use quickcheck::quickcheck;
    use std::io::Cursor;

    fn create_test_index<R: BufRead>(reader: R) -> Result<Index> {
        let mut index = Index::new();
        let file_id = index.interned_paths.intern("test_file.txt");
        Index::add_file_to_index(&mut index, reader, file_id)?;
        Ok(index)
    }

    #[test]
    fn test_empty_content() {
        let reader = Cursor::new(vec![]);
        assert!(create_test_index(reader).is_err());
    }

    #[test]
    fn test_content_with_exactly_three_valid_chars() {
        let reader = Cursor::new("abc");
        let index = create_test_index(reader).unwrap();
        assert_eq!(
            index.trigrams.len(),
            1,
            "Index should contain exactly one trigram for three valid characters"
        );
        assert!(index.trigrams.contains_key(&[b'a', b'b', b'c']));
    }

    #[test]
    fn test_content_with_multiple_trigrams() {
        let reader = Cursor::new("the quick brown fox");
        let index = create_test_index(reader).unwrap();
        assert!(index.trigrams.contains_key(&[b't', b'h', b'e']));
        assert!(index.trigrams.contains_key(&[b'h', b'e', b' ']));
        assert!(index.trigrams.contains_key(&[b'q', b'u', b'i']));
    }

    #[test]
    fn test_content_with_whitespace() {
        let reader = Cursor::new("the\n\tquick\rfox");
        let index = create_test_index(reader).unwrap();
        assert!(index.trigrams.contains_key(&[b't', b'h', b'e']));
        assert!(index.trigrams.contains_key(&[b'q', b'u', b'i']));
        assert!(index.trigrams.contains_key(&[b'u', b'i', b'c']));
    }

    #[test]
    fn test_line_tracking() {
        let reader = Cursor::new("first\nsecond\nthird");
        let index = create_test_index(reader).unwrap();

        let file_id = index.interned_paths.map.get("test_file.txt").unwrap();
        let lines = index.lines.get(file_id).unwrap();

        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0].start, 0);
        assert_eq!(lines[0].end, 5);
        assert_eq!(lines[1].start, 6);
        assert_eq!(lines[1].end, 12);
        assert_eq!(lines[2].start, 13);
        assert_eq!(lines[2].end, 18);
    }

    #[test]
    fn test_large_file() {
        let content = "a".repeat(1_000_000);
        let reader = Cursor::new(content);
        let index = create_test_index(reader).unwrap();
        assert!(!index.trigrams.is_empty());
    }

    #[test]
    fn test_posting_correctness() {
        let reader = Cursor::new("hello\nworld");
        let index = create_test_index(reader).unwrap();
        let hel = index.trigrams.get(&[b'h', b'e', b'l']).unwrap();
        assert_eq!(hel.get(0).unwrap().line_number, 0);
        assert_eq!(hel.get(0).unwrap().byte_offset, 2);

        let wor = index.trigrams.get(&[b'w', b'o', b'r']).unwrap();
        assert_eq!(wor.get(0).unwrap().line_number, 1);
        assert_eq!(wor.get(0).unwrap().byte_offset, 8);
    }

    #[test]
    fn test_daachorse_trigram_matching() {
        let test_content = b"the quick brown fox";
        let reader = Cursor::new(test_content);
        let index = create_test_index(reader).unwrap();

        let patterns: Vec<&[u8]> =
            index.trigrams.keys().map(|key| key.as_ref()).collect();
        let pma: DoubleArrayAhoCorasick<usize> =
            DoubleArrayAhoCorasick::new(patterns).unwrap();

        let matches: Vec<_> = pma.find_overlapping_iter(b"quick").collect();

        assert!(
            !matches.is_empty(),
            "Should find at least one trigram match"
        );

        // the word "quick" should contain trigrams: "qui", "uic", "ick"
        let match_count = matches.len();
        assert_eq!(match_count, 3, "Expected 3 trigrams in 'quick'");
    }

    // property-based testing using QuickCheck
    quickcheck! {
        fn test_no_trigrams_for_short_content(content: Vec<u8>) -> bool {
            let reader = Cursor::new(content.clone());
            match create_test_index(reader) {
                Ok(index) => {
                    // if we got an index, ensure it's empty for short content
                    if content.len() < 3 {
                        index.trigrams.is_empty()
                    } else {
                        true // for longer content, any result is fine
                    }
                }
                Err(_) => content.len() < 3 // error is expected for short content
            }
        }

        fn test_trigrams_are_correctly_generated(content: Vec<u8>) -> bool {
            let reader = Cursor::new(content.clone());

            // skip validation for content that's too short
            if content.len() < 3 {
                return true;
            }

            let index = match create_test_index(reader) {
                Ok(idx) => idx,
                Err(_) => return true // skip validation if indexing fails
            };

            let expected_trigrams: Vec<Trigram> = content
                .windows(3)
                .map(|w| [w[0], w[1], w[2]])
                .collect();

            let indexed_trigrams: Vec<_> = index.trigrams.keys().collect();

            indexed_trigrams.iter().all(|t| expected_trigrams.contains(t)) &&
                expected_trigrams.iter().all(|t| index.trigrams.contains_key(t))
        }
    }

    #[test]
    fn test_integration_index_and_search() {
        let test_files = vec![
            ("file1.txt", "the quick brown fox jumps over the lazy dog"),
            (
                "file2.txt",
                "hello world\nthis is a test\nwith multiple lines",
            ),
            (
                "file3.txt",
                "searching for patterns\nin trigram indices\nis quite efficient",
            ),
        ];

        let mut index = Index::new();

        for (filename, content) in &test_files {
            let file_id = index.interned_paths.intern(filename);
            let reader = std::io::Cursor::new(content.as_bytes());
            Index::add_file_to_index(&mut index, reader, file_id).unwrap();
        }

        let temp_path = std::env::temp_dir().join("test_index.bin");
        index.store(&temp_path).unwrap();

        let index_view = IndexView::try_from(temp_path.as_path()).unwrap();

        let results = index_view.search("the").unwrap();
        assert!(!results.is_empty(), "Should find matches for 'the'");

        let file1_matches: Vec<_> = results
            .iter()
            .filter(|r| r.file_path.contains("file1.txt"))
            .collect();
        assert!(!file1_matches.is_empty(), "Should find 'the' in file1.txt");

        let test_results = index_view.search("test").unwrap();
        assert!(!test_results.is_empty(), "Should find matches for 'test'");

        let file2_matches: Vec<_> = test_results
            .iter()
            .filter(|r| r.file_path.contains("file2.txt"))
            .collect();
        assert!(!file2_matches.is_empty(), "Should find 'test' in file2.txt");

        let trigram_results = index_view.search("trigram").unwrap();
        assert!(
            !trigram_results.is_empty(),
            "Should find matches for 'trigram'"
        );

        let file3_matches: Vec<_> = trigram_results
            .iter()
            .filter(|r| r.file_path.contains("file3.txt"))
            .collect();
        assert!(
            !file3_matches.is_empty(),
            "Should find 'trigram' in file3.txt"
        );

        let no_results = index_view.search("xyz123").unwrap();
        assert!(
            no_results.is_empty(),
            "Should find no matches for non-existent pattern"
        );

        let world_results = index_view.search("world").unwrap();
        assert!(!world_results.is_empty(), "Should find matches for 'world'");

        for result in &world_results {
            if result.file_path.contains("file2.txt") {
                assert_eq!(
                    result.line_number, 0,
                    "Should find 'world' on line 0 of file2.txt"
                );
            }
        }

        for result in &results {
            assert!(
                result.match_start <= result.match_end,
                "Match start should be <= match end"
            );
            assert!(
                result.match_end > result.match_start,
                "Match should have non-zero length"
            );
        }

        std::fs::remove_file(&temp_path).ok();

        println!(
            "Integration test passed! Found {} total search results across all queries",
            results.len()
                + test_results.len()
                + trigram_results.len()
                + world_results.len()
        );
    }

    #[test]
    fn test_integration_edge_cases() {
        let mut index = Index::new();

        let file_id1 = index.interned_paths.intern("newlines.txt");
        let reader1 = std::io::Cursor::new("\n\n\n");
        Index::add_file_to_index(&mut index, reader1, file_id1).unwrap();

        let file_id_short = index.interned_paths.intern("too_short.txt");
        let reader_short = std::io::Cursor::new("ab");
        assert!(
            Index::add_file_to_index(&mut index, reader_short, file_id_short)
                .is_err(),
            "Should fail for content shorter than 3 bytes"
        );

        let file_id2 = index.interned_paths.intern("minimal.txt");
        let reader2 = std::io::Cursor::new("abc");
        Index::add_file_to_index(&mut index, reader2, file_id2).unwrap();

        let file_id3 = index.interned_paths.intern("special.txt");
        let reader3 = std::io::Cursor::new("café naïve résumé");
        Index::add_file_to_index(&mut index, reader3, file_id3).unwrap();

        let temp_path = std::env::temp_dir().join("test_edge_index.bin");
        index.store(&temp_path).unwrap();
        let index_view = IndexView::try_from(temp_path.as_path()).unwrap();

        let abc_results = index_view.search("abc").unwrap();
        assert!(!abc_results.is_empty(), "Should find 'abc' in minimal file");

        let cafe_results = index_view.search("café").unwrap();
        assert!(
            !cafe_results.is_empty(),
            "Should find 'café' in special characters file"
        );

        std::fs::remove_file(&temp_path).ok();
    }
}
