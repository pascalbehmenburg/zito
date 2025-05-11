mod treesitter;
pub use treesitter::extract_symbols;

use eyre::{Result, eyre};
use flate2::Compression;
use rkyv::{
    Archive, Deserialize, Serialize, access_unchecked, api::serialize_using,
    ser::writer::IoWriter, util::with_arena,
};
use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, Read, Write},
    ops::Deref,
    path::{Path, PathBuf},
};
use walkdir::WalkDir;

pub type FileId = u32;
pub type Offset = u32;
pub type Trigram = [u8; 3];

#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct Line {
    pub start: Offset,
    pub end: Offset,
}

#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct Posting {
    pub line_number: u32,
    pub byte_offset: u32,
}

#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct PostingList {
    data: Vec<Posting>,
}

impl PostingList {
    #[inline]
    fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    #[inline]
    fn push(&mut self, posting: Posting) {
        self.data.push(posting);
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&Posting> {
        self.data.get(index)
    }

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

#[derive(Archive, Debug, Deserialize, Serialize)]
#[rkyv(derive(Debug))]
pub struct Index {
    pub file_path: String,
    pub lines: Vec<Line>,
    pub trigrams: HashMap<Trigram, PostingList>,
}

pub struct IndexView {
    buf: Vec<u8>,
}

impl IndexView {
    fn decompress(file: File) -> impl Read {
        flate2::read::ZlibDecoder::new(file)
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

    fn try_from(path: &Path) -> Result<Self> {
        let mut reader = Self::decompress(File::open(path)?);
        let mut buf = Vec::new();
        reader.read_to_end(&mut buf)?;
        Ok(IndexView { buf })
    }
}

impl Index {
    fn compress(writer: impl Write) -> impl Write {
        flate2::write::ZlibEncoder::new(writer, Compression::best())
    }

    pub fn store<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let mut compressor = Self::compress(File::create(path).unwrap());
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
}

pub fn index_files<P: AsRef<Path>>(path: P) -> Result<Vec<Index>> {
    let mut indices = Vec::new();

    for entry in WalkDir::new(path)
        .follow_links(true)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
    {
        if let Ok(file) = std::fs::File::open(entry.path()) {
            let reader = std::io::BufReader::new(file);
            match index_file(reader, entry.path().to_path_buf()) {
                Ok(index) => indices.push(index),
                Err(_) => continue,
            }
        }
    }

    Ok(indices)
}

pub fn index_file<R: BufRead>(
    mut reader: R,
    file_path: PathBuf,
) -> Result<Index> {
    let mut content =
        Vec::with_capacity(reader.fill_buf().map_or(0, |b| b.len()));
    reader.read_to_end(&mut content)?;

    if content.len() < 3 {
        return Err(eyre!("File too short to index"));
    }

    // Calculate expected number of unique trigrams
    // Assuming ASCII text, each byte can be one of ~95 printable chars
    // For a text of length n, we have (n-2) trigrams
    // Estimate unique trigrams as min(actual_trigrams, max_possible_trigrams)
    let num_trigrams = content.len().saturating_sub(2);
    // ~857k max possible unique trigrams
    let max_possible_trigrams = 95 * 95 * 95;
    let estimated_unique = (num_trigrams / 2).min(max_possible_trigrams);

    let mut index = Index {
        file_path: file_path.to_string_lossy().to_string(),
        // Assume average line length of 80
        lines: Vec::with_capacity((content.len() / 40).max(1024)),
        trigrams: HashMap::with_capacity(estimated_unique),
    };

    let mut line_number = 0;
    let mut line_start = 0;
    const POSTING_CAPACITY: usize = 64;
    let mut posting_lists: HashMap<Trigram, PostingList> =
        HashMap::with_capacity(estimated_unique);

    let window = content.as_ptr();
    let end = unsafe { window.add(content.len()) };
    let mut current = window;

    while unsafe { current.add(2) } < end {
        let c = unsafe { *current };

        if c == b'\n' {
            index.lines.push(Line {
                start: line_start,
                end: (current as usize - window as usize) as Offset,
            });
            line_number += 1;
            line_start = (current as usize - window as usize + 1) as Offset;
        }

        let trigram = unsafe { [*current, *current.add(1), *current.add(2)] };

        posting_lists
            .entry(trigram)
            .or_insert_with(|| PostingList::with_capacity(POSTING_CAPACITY))
            .push(Posting {
                line_number,
                byte_offset: unsafe {
                    current.add(2) as usize - window as usize
                } as Offset,
            });

        current = unsafe { current.add(1) };
    }

    // Handle the last line if it doesn't end with a newline
    let final_offset =
        unsafe { current.add(2).min(end) as usize - window as usize };
    if line_start as usize != final_offset {
        index.lines.push(Line {
            start: line_start,
            end: final_offset as Offset,
        });
    }

    index.trigrams = posting_lists;
    Ok(index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use daachorse::DoubleArrayAhoCorasick;
    use quickcheck::quickcheck;
    use std::io::Cursor;

    #[test]
    fn test_empty_content() {
        let reader = Cursor::new(vec![]);
        assert!(index_file(reader, PathBuf::new()).is_err());
    }

    #[test]
    fn test_content_with_exactly_three_valid_chars() {
        let reader = Cursor::new("abc");
        let index = index_file(reader, PathBuf::new()).unwrap();
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
        let index = index_file(reader, PathBuf::new()).unwrap();
        assert!(index.trigrams.contains_key(&[b't', b'h', b'e']));
        assert!(index.trigrams.contains_key(&[b'h', b'e', b' ']));
        assert!(index.trigrams.contains_key(&[b'q', b'u', b'i']));
    }

    #[test]
    fn test_content_with_whitespace() {
        let reader = Cursor::new("the\n\tquick\rfox");
        let index = index_file(reader, PathBuf::new()).unwrap();
        assert!(index.trigrams.contains_key(&[b't', b'h', b'e']));
        assert!(index.trigrams.contains_key(&[b'q', b'u', b'i']));
        assert!(index.trigrams.contains_key(&[b'u', b'i', b'c']));
    }

    #[test]
    fn test_line_tracking() {
        let reader = Cursor::new("first\nsecond\nthird");
        let index = index_file(reader, PathBuf::new()).unwrap();
        assert_eq!(index.lines.len(), 3);
        assert_eq!(index.lines[0].start, 0);
        assert_eq!(index.lines[0].end, 5);
        assert_eq!(index.lines[1].start, 6);
        assert_eq!(index.lines[1].end, 12);
        assert_eq!(index.lines[2].start, 13);
        assert_eq!(index.lines[2].end, 18);
    }

    #[test]
    fn test_large_file() {
        let content = "a".repeat(1_000_000);
        let reader = Cursor::new(content);
        let index = index_file(reader, PathBuf::new()).unwrap();
        assert!(!index.trigrams.is_empty());
    }

    #[test]
    fn test_posting_correctness() {
        let reader = Cursor::new("hello\nworld");
        let index = index_file(reader, PathBuf::new()).unwrap();
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
        let index = index_file(reader, PathBuf::new()).unwrap();

        // Convert trigrams to patterns for Aho-Corasick
        let patterns: Vec<&[u8]> =
            index.trigrams.keys().map(|key| key.as_ref()).collect();
        let pma: DoubleArrayAhoCorasick<usize> =
            DoubleArrayAhoCorasick::new(patterns).unwrap();

        // Search for matches in "quick"
        let matches: Vec<_> = pma.find_overlapping_iter(b"quick").collect();

        assert!(
            !matches.is_empty(),
            "Should find at least one trigram match"
        );

        // The word "quick" should contain trigrams: "qui", "uic", "ick"
        let match_count = matches.len();
        assert_eq!(match_count, 3, "Expected 3 trigrams in 'quick'");
    }

    // Property-based testing using QuickCheck
    quickcheck! {
        fn test_no_trigrams_for_short_content(content: Vec<u8>) -> bool {
            let reader = Cursor::new(content.clone());
            match index_file(reader, PathBuf::new()) {
                Ok(index) => {
                    // If we got an index, ensure it's empty for short content
                    if content.len() < 3 {
                        index.trigrams.is_empty()
                    } else {
                        true // For longer content, any result is fine
                    }
                }
                Err(_) => content.len() < 3 // Error is expected for short content
            }
        }

        fn test_trigrams_are_correctly_generated(content: Vec<u8>) -> bool {
            let reader = Cursor::new(content.clone());

            // Skip validation for content that's too short
            if content.len() < 3 {
                return true;
            }

            // Try to create index
            let index = match index_file(reader, PathBuf::new()) {
                Ok(idx) => idx,
                Err(_) => return true // Skip validation if indexing fails
            };

            // Create expected trigrams directly from content
            let expected_trigrams: Vec<Trigram> = content
                .windows(3)
                .map(|w| [w[0], w[1], w[2]])
                .collect();

            // Verify all indexed trigrams are expected and all expected trigrams are indexed
            let indexed_trigrams: Vec<_> = index.trigrams.keys().collect();

            // Both conditions must be true for the test to pass
            indexed_trigrams.iter().all(|t| expected_trigrams.contains(t)) &&
                expected_trigrams.iter().all(|t| index.trigrams.contains_key(t))
        }
    }
}
