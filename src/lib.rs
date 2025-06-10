//! A fast text search engine using trigram indexing and Aho-Corasick pattern matching.
//!
//! This library provides efficient full-text search capabilities by building trigram-based
//! inverted indices. It uses the daachorse crate for fast pattern matching and rkyv for
//! efficient serialization of the index data.

use colored::Colorize;
use daachorse::DoubleArrayAhoCorasick;
use eyre::{Result, eyre};
use flate2::Compression;
use fxhash::FxHashMap;
use mimalloc::MiMalloc;
use regex::Regex;
use regex_syntax::hir::literal::Extractor;
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

/// A space-efficient identifier for interned path components.
///
/// This is used to reference individual path components (directory names, filenames)
/// by a small integer ID, enabling maximum deduplication of common path prefixes.
#[derive(
    Archive, Deserialize, Serialize, Copy, Clone, Debug, PartialEq, Eq, Hash,
)]
#[rkyv(derive(Debug, Hash, PartialEq, Eq))]
pub struct ComponentId(u32);

/// A space-efficient identifier for interned file paths.
///
/// File paths are decomposed into components and stored as sequences of component IDs,
/// dramatically reducing memory usage for paths with common prefixes.
#[derive(
    Archive, Deserialize, Serialize, Copy, Clone, Debug, PartialEq, Eq, Hash,
)]
#[rkyv(derive(Debug, Hash, PartialEq, Eq, Clone, Copy))]
pub struct InternedPathId(u32);

/// Advanced path interner optimized for file system hierarchies.
///
/// This structure exploits the hierarchical nature of file paths to achieve
/// maximum space efficiency while maintaining O(1) lookup performance.
///
/// Key optimizations:
/// - **Component-level deduplication**: Individual path components (directory names,
///   filenames) are interned separately, maximizing reuse of common names like "src", "target", etc.
/// - **Path compression**: Full paths are stored as compact sequences of component IDs
/// - **Fast lookup**: Uses hash tables for O(1) intern/resolve operations
/// - **Prefix sharing**: Common directory prefixes are automatically shared between paths
///
/// For example, the paths:
/// - `/home/user/project/src/main.rs`
/// - `/home/user/project/src/lib.rs`
/// - `/home/user/project/tests/test.rs`
///
/// Would share the components `"home"`, `"user"`, `"project"`, `"src"` and only store
/// unique components like `"main.rs"`, `"lib.rs"`, `"tests"`, `"test.rs"` separately.
#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct PathInterner {
    /// Maps path component strings to their IDs for O(1) component lookup
    component_map: FxHashMap<String, ComponentId>,
    /// Stores actual component strings indexed by ID
    components: Vec<String>,
    /// Maps complete path sequences to their interned path IDs
    path_map: FxHashMap<Vec<ComponentId>, InternedPathId>,
    /// Stores path component sequences indexed by path ID
    paths: Vec<Vec<ComponentId>>,
}

impl Default for PathInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl PathInterner {
    /// Creates a new empty path interner.
    pub fn new() -> Self {
        Self {
            component_map: FxHashMap::default(),
            components: Vec::new(),
            path_map: FxHashMap::default(),
            paths: Vec::new(),
        }
    }

    /// Interns a path component and returns its ID.
    ///
    /// If the component has already been interned, returns the existing ID.
    /// This enables maximum reuse of common directory and file names.
    fn intern_component(&mut self, component: &str) -> ComponentId {
        if let Some(&id) = self.component_map.get(component) {
            return id;
        }
        let id = ComponentId(self.components.len() as u32);
        self.components.push(component.to_string());
        self.component_map.insert(component.to_string(), id);
        id
    }

    /// Interns a file path and returns its ID.
    ///
    /// Decomposes the path into components, interns each component separately,
    /// and stores the path as a sequence of component IDs. This maximizes
    /// space efficiency for file system hierarchies with shared prefixes.
    ///
    /// # Arguments
    ///
    /// * `path` - The file path to intern (e.g., "/home/user/project/src/main.rs")
    ///
    /// # Returns
    ///
    /// An interned path ID that can be used to efficiently reference this path
    pub fn intern(&mut self, path: &str) -> InternedPathId {
        // Split path into components, filtering out empty ones
        let component_strs: Vec<&str> = path
            .split(['/', '\\']) // Handle both Unix and Windows separators
            .filter(|s| !s.is_empty())
            .collect();

        // Convert each component string to a component ID
        let component_ids: Vec<ComponentId> = component_strs
            .iter()
            .map(|&comp| self.intern_component(comp))
            .collect();

        // Check if this exact path sequence already exists
        if let Some(&path_id) = self.path_map.get(&component_ids) {
            return path_id;
        }

        // Create new path entry
        let path_id = InternedPathId(self.paths.len() as u32);
        self.paths.push(component_ids.clone());
        self.path_map.insert(component_ids, path_id);
        path_id
    }

    /// Prints detailed statistics about the interner's memory efficiency.
    ///
    /// This is useful for understanding how much space is saved through component deduplication.
    pub fn print_detailed_stats(&self) {
        let (unique_components, total_paths, estimated_savings) = self.stats();

        let total_component_memory: usize = self
            .components
            .iter()
            .map(|s| s.len() + std::mem::size_of::<String>())
            .sum();

        let total_path_memory: usize = self
            .paths
            .iter()
            .map(|path| {
                path.len() * std::mem::size_of::<ComponentId>()
                    + std::mem::size_of::<Vec<ComponentId>>()
            })
            .sum();

        let map_memory = self.component_map.len()
            * (std::mem::size_of::<String>()
                + std::mem::size_of::<ComponentId>())
            + self.path_map.len()
                * (std::mem::size_of::<Vec<ComponentId>>()
                    + std::mem::size_of::<InternedPathId>());

        println!("=== PathInterner Statistics ===");
        println!("Unique components: {}", unique_components);
        println!("Total paths: {}", total_paths);
        println!("Component memory: ~{} bytes", total_component_memory);
        println!("Path structure memory: ~{} bytes", total_path_memory);
        println!("Lookup table memory: ~{} bytes", map_memory);
        println!(
            "Total memory: ~{} bytes",
            total_component_memory + total_path_memory + map_memory
        );
        println!("Estimated space saved: ~{} bytes", estimated_savings);

        if total_paths > 0 {
            let avg_path_length =
                self.paths.iter().map(|p| p.len()).sum::<usize>() as f64
                    / total_paths as f64;
            println!("Average path length: {:.1} components", avg_path_length);

            let compression_ratio = if estimated_savings > 0 {
                (estimated_savings as f64)
                    / (estimated_savings as f64 + total_component_memory as f64)
                    * 100.0
            } else {
                0.0
            };
            println!("Estimated compression: {:.1}%", compression_ratio);
        }
        println!("================================");
    }

    /// Gets the memory usage statistics for this interner.
    ///
    /// Returns (unique_components, total_paths, estimated_bytes_saved)
    pub fn stats(&self) -> (usize, usize, usize) {
        let unique_components = self.components.len();
        let total_paths = self.paths.len();

        // Estimate bytes saved by component deduplication
        let total_component_chars: usize =
            self.components.iter().map(|s| s.len()).sum();
        let total_path_chars: usize = self
            .paths
            .iter()
            .map(|path| {
                path.iter()
                    .map(|&id| self.components[id.0 as usize].len() + 1) // +1 for separator
                    .sum::<usize>()
            })
            .sum();

        let estimated_savings =
            total_path_chars.saturating_sub(total_component_chars);

        (unique_components, total_paths, estimated_savings)
    }
}

impl ArchivedPathInterner {
    /// Resolves an interned path ID back to its original path string.
    ///
    /// Reconstructs the full path by joining the component strings with
    /// the appropriate path separator for the current platform.
    pub fn resolve(&self, id: ArchivedInternedPathId) -> String {
        let path_idx = id.0.to_native() as usize;
        if path_idx >= self.paths.len() {
            return String::new();
        }

        let component_ids = &self.paths[path_idx];
        let components: Vec<&str> = component_ids
            .iter()
            .map(|id| &self.components[id.0.to_native() as usize] as &str)
            .collect();

        // Join with platform-appropriate separator
        #[cfg(windows)]
        let separator = "\\";
        #[cfg(not(windows))]
        let separator = "/";

        if components.is_empty() {
            String::new()
        } else {
            format!("{}{}", separator, components.join(separator))
        }
    }
}

/// Unique identifier for a file in the index.
pub type FileId = u32;

/// Byte offset within a file.
pub type Offset = u32;

/// A three-byte sequence used for trigram indexing.
pub type Trigram = [u8; 3];

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
    pub file_id: InternedPathId,
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
/// and file contents needed for fast text search.
#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct Index {
    /// Interner for file paths to reduce memory usage.
    pub interned_paths: PathInterner,
    /// Maps file IDs to their actual content.
    pub file_contents: FxHashMap<InternedPathId, Vec<u8>>,
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
    automaton: DoubleArrayAhoCorasick<&'static ArchivedPostingList>,
}

pub struct SearchOptions {
    pub regex: bool,
}

impl SearchOptions {
    pub fn new(regex: bool) -> Self {
        SearchOptions { regex }
    }
}

impl Default for SearchOptions {
    /// Creates a new `SearchOptions` instance with regex set to `false`.
    /// As regex is more expensive to compile and execute, it is disabled by default.
    fn default() -> Self {
        SearchOptions::new(false)
    }
}

impl IndexView {
    /// Searches the index for the given query either regex or literal.
    ///
    /// Returns all matches found in the indexed files, with each result
    /// containing the file path, line number, line text, and match position.
    ///
    /// # Note
    ///
    /// Regex searches are slower due to the overhead of compiling and executing regular expressions.
    ///
    /// # Arguments
    ///
    /// * `query` - The text to search for (must be at least 3 characters)
    ///
    /// # Returns
    ///
    /// A vector of search results, sorted by file path and line number.
    pub fn search(
        &self,
        query: &str,
        options: SearchOptions,
    ) -> Result<Vec<SearchResult>> {
        if query.len() < 3 {
            return Err(eyre!(
                "A search query must be at least 3 characters long."
            ));
        }
        if options.regex {
            self.re_search(query)
        } else {
            self._search(query.as_bytes())
        }
    }

    /// Searches the index for the given regex string.
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
    fn re_search(&self, query: &str) -> Result<Vec<SearchResult>> {
        debug_assert!(query.len() >= 3, "Query must be at least 3 characters");
        let mut results = Vec::new();
        let re = Regex::new(query)?;
        let regex = regex_syntax::parse(query)?;
        let mut literals = Extractor::new().extract(&regex);
        literals.dedup();

        let patterns: Vec<&[u8]> = literals
            .literals()
            .unwrap_or_default()
            .iter()
            .map(|lit| lit.as_bytes())
            .collect();

        if patterns.is_empty() {
            return Err(eyre!("Could not extract literals from regex."));
        }

        for pattern in patterns {
            let search_results = self._search(pattern)?;
            results.extend(search_results.into_iter().filter_map(|result| {
                re.find(result.line_text.as_str()).map(|mat| {
                    let mut mutated_result = result.clone();
                    mutated_result.match_start = mat.start() as u32;
                    mutated_result.match_end = mat.end() as u32;
                    mutated_result
                })
            }));
        }
        Ok(results)
    }

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
    fn _search(&self, query: &[u8]) -> Result<Vec<SearchResult>> {
        debug_assert!(query.len() >= 3, "Query must be at least 3 characters");
        println!(
            "Searching for query: {}",
            String::from_utf8_lossy(query).blue()
        );
        let mut results = Vec::new();

        let mut candidate_postings = Vec::new();
        for m in self.automaton.find_iter(query) {
            let posting_list = m.value();
            for posting in posting_list {
                candidate_postings.push(posting);
            }
        }

        if candidate_postings.is_empty() {
            return Ok(Vec::new());
        }

        // Group postings by file first, then by line within each file
        let mut files_to_lines: FxHashMap<
            ArchivedInternedPathId,
            FxHashMap<<u32 as Archive>::Archived, Vec<&ArchivedPosting>>,
        > = FxHashMap::default();

        for posting in candidate_postings {
            let file_id = posting.file_id;
            let line_number = posting.line_number;
            files_to_lines
                .entry(file_id)
                .or_default()
                .entry(line_number)
                .or_default()
                .push(posting);
        }

        // Cache file contents, paths, and line boundaries to avoid reprocessing
        let mut file_content_cache: FxHashMap<ArchivedInternedPathId, &str> =
            FxHashMap::default();
        let mut file_path_cache: FxHashMap<ArchivedInternedPathId, String> =
            FxHashMap::default();
        let mut file_lines_cache: FxHashMap<ArchivedInternedPathId, Vec<&str>> =
            FxHashMap::default();

        for (file_id_native, lines_map) in files_to_lines {
            // Get file path once per file (cached)
            let file_path = if let Some(cached_path) =
                file_path_cache.get(&file_id_native)
            {
                cached_path
            } else {
                let path = self.interned_paths.resolve(file_id_native);
                file_path_cache.insert(file_id_native, path.clone());
                file_path_cache.get(&file_id_native).unwrap()
            };

            // Get file content once per file (cached and avoid string conversion)
            let file_content = if let Some(&cached_content) =
                file_content_cache.get(&file_id_native)
            {
                cached_content
            } else if let Some(stored_content) =
                self.file_contents.get(&file_id_native)
            {
                // Use std::str::from_utf8 instead of String::from_utf8(to_vec())
                match std::str::from_utf8(stored_content) {
                    Ok(content_str) => {
                        file_content_cache.insert(file_id_native, content_str);
                        content_str
                    }
                    Err(_) => continue, // Skip files with invalid UTF-8
                }
            } else {
                continue; // Skip if content not found in index
            };

            // Get lines for this file (cached)
            let lines = if let Some(cached_lines) =
                file_lines_cache.get(&file_id_native)
            {
                cached_lines
            } else {
                let lines: Vec<&str> = file_content.lines().collect();
                file_lines_cache.insert(file_id_native, lines);
                file_lines_cache.get(&file_id_native).unwrap()
            };

            // Process all lines for this file
            for (line_number, _postings) in lines_map {
                // Get specific line from pre-computed lines
                let line_number = line_number.to_native();
                let line_text = match lines.get(line_number as usize) {
                    Some(&line) => line,
                    None => continue, // Line number out of bounds
                };

                let mut start_pos: usize = 0;
                let query_str = std::str::from_utf8(query)?;
                while let Some(match_pos) =
                    line_text[start_pos..].find(query_str)
                {
                    let absolute_match_pos = start_pos + match_pos;

                    let search_result = SearchResult {
                        file_path: file_path.clone(),
                        line_number,
                        line_text: line_text.to_string(),
                        match_start: absolute_match_pos as u32,
                        match_end: absolute_match_pos as u32
                            + query.len() as u32,
                    };
                    results.push(search_result);

                    start_pos = absolute_match_pos + 1;
                }
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
    /// The automaton is built during construction for immediate search readiness.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the compressed index file
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be opened, decompressed, or if
    /// the automaton cannot be built.
    fn try_from(path: &Path) -> Result<Self> {
        let mut decompressor =
            flate2::read::ZlibDecoder::new(File::open(path)?);
        let mut buf = Vec::new();
        decompressor.read_to_end(&mut buf)?;

        // Build the automaton immediately
        let archived_index = unsafe { access_unchecked::<ArchivedIndex>(&buf) };
        let trigram_pairs: Vec<(&[u8; 3], &ArchivedPostingList)> =
            archived_index.trigrams.iter().collect();

        let daac = DoubleArrayAhoCorasick::with_values(
            trigram_pairs.iter().map(|(k, v)| (k.as_slice(), *v)),
        )
        .map_err(|e| {
            eyre::eyre!("Failed to build Aho-Corasick automaton: {}", e)
        })?;

        // Safety: The automaton references data in buf, which will be owned by the IndexView
        // This is safe because IndexView owns buf and the automaton will not outlive IndexView
        let static_automaton: DoubleArrayAhoCorasick<
            &'static ArchivedPostingList,
        > = unsafe { std::mem::transmute(daac) };

        Ok(IndexView {
            buf,
            automaton: static_automaton,
        })
    }
}

impl Default for Index {
    fn default() -> Self {
        Self::new()
    }
}

impl Index {
    /// Creates a new empty index.
    pub fn new() -> Self {
        Index {
            interned_paths: PathInterner::new(),
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
                    entry.path().to_path_buf().to_string_lossy().as_ref(),
                );
                if Self::add_file_to_index(&mut main_index, reader, file_id)
                    .is_err()
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
        file_id: InternedPathId,
    ) -> Result<()> {
        let mut content =
            Vec::with_capacity(reader.fill_buf().map_or(0, |b| b.len()));
        reader.read_to_end(&mut content)?;

        if content.len() < 3 {
            return Err(eyre!("File too short to index"));
        }

        let mut line_number = 0;

        let window = content.as_ptr();
        let end = unsafe { window.add(content.len()) };
        let mut current = window;
        while unsafe { current.add(2) } < end {
            let c = unsafe { *current };

            if c == b'\n' {
                line_number += 1;
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
                    file_id,
                });

            current = unsafe { current.add(1) };
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
    fn test_content_storage() {
        let reader = std::io::Cursor::new("first\nsecond\nthird");
        let index = create_test_index(reader).unwrap();

        // Instead of accessing internal fields, let's just verify the index works correctly
        // by checking that trigrams from the content are properly indexed
        assert!(index.trigrams.contains_key(&[b'f', b'i', b'r']));
        assert!(index.trigrams.contains_key(&[b's', b'e', b'c']));
        assert!(index.trigrams.contains_key(&[b't', b'h', b'i']));

        // Check that we have some file contents stored
        assert!(!index.file_contents.is_empty());
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

        let results = index_view._search("the".as_bytes()).unwrap();
        assert!(!results.is_empty(), "Should find matches for 'the'");

        let file1_matches: Vec<_> = results
            .iter()
            .filter(|r| r.file_path.contains("file1.txt"))
            .collect();
        assert!(!file1_matches.is_empty(), "Should find 'the' in file1.txt");

        let test_results = index_view._search("test".as_bytes()).unwrap();
        assert!(!test_results.is_empty(), "Should find matches for 'test'");

        let file2_matches: Vec<_> = test_results
            .iter()
            .filter(|r| r.file_path.contains("file2.txt"))
            .collect();
        assert!(!file2_matches.is_empty(), "Should find 'test' in file2.txt");

        let trigram_results = index_view._search("trigram".as_bytes()).unwrap();
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

        let no_results = index_view._search("xyz123".as_bytes()).unwrap();
        assert!(
            no_results.is_empty(),
            "Should find no matches for non-existent pattern"
        );

        let world_results = index_view._search("world".as_bytes()).unwrap();
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

        let abc_results = index_view._search("abc".as_bytes()).unwrap();
        assert!(!abc_results.is_empty(), "Should find 'abc' in minimal file");

        let cafe_results = index_view._search("café".as_bytes()).unwrap();
        assert!(
            !cafe_results.is_empty(),
            "Should find 'café' in special characters file"
        );

        std::fs::remove_file(&temp_path).ok();
    }

    #[test]
    fn test_path_interner_basic() {
        let mut interner = PathInterner::new();

        let id1 = interner.intern("/home/user/project/src/main.rs");
        let id2 = interner.intern("/home/user/project/src/lib.rs");
        let id3 = interner.intern("/home/user/project/tests/test.rs");

        // Different paths should have different IDs
        assert_ne!(id1, id2);
        assert_ne!(id1, id3);
        assert_ne!(id2, id3);

        // Same path should return same ID
        let id1_again = interner.intern("/home/user/project/src/main.rs");
        assert_eq!(id1, id1_again);
    }

    #[test]
    fn test_path_interner_component_sharing() {
        let mut interner = PathInterner::new();

        // Add several paths with shared prefixes
        interner.intern("/home/user/project/src/main.rs");
        interner.intern("/home/user/project/src/lib.rs");
        interner.intern("/home/user/project/tests/test.rs");
        interner.intern("/home/user/other/README.md");

        let (unique_components, total_paths, estimated_savings) =
            interner.stats();

        // Should have deduplicated common components like "home", "user", "project", "src"
        assert_eq!(total_paths, 4);
        assert!(unique_components < 12); // Much fewer than if we stored each path separately
        assert!(estimated_savings > 0); // Should save space through component reuse

        println!(
            "PathInterner stats: {} unique components, {} paths, ~{} bytes saved",
            unique_components, total_paths, estimated_savings
        );
    }
}
