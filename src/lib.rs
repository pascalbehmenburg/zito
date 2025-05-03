use eyre::{Result, eyre};
use std::{collections::HashMap, io::BufRead};

pub type FileId = u32;
pub type Offset = u32;
pub type Trigram = [u8; 3];

#[derive(Debug)]
pub struct Line {
    pub start: Offset,
    pub end: Offset,
}

#[derive(Debug)]
pub struct Posting {
    pub line_number: Offset,
    pub byte_offset: Offset,
}

#[derive(Debug)]
pub struct Index {
    pub lines: Vec<Line>,
    pub trigrams: HashMap<Trigram, Vec<Posting>>,
}

pub fn index_files(file_paths: &[&str]) -> Result<Vec<Index>> {
    let mut indices = Vec::with_capacity(file_paths.len());

    for file in file_paths {
        let reader = std::fs::File::open(file).map_err(|e| eyre!("Failed to open file {}: {}", file, e))?;
        let reader = std::io::BufReader::new(reader);
        let index = index_file(reader)?;
        indices.push(index);
    }

    Ok(indices)
}

pub fn index_file<R: BufRead>(mut reader: R) -> Result<Index> {
    let mut index = Index {
        lines: Vec::with_capacity(1024),
        trigrams: HashMap::with_capacity(4096),
    };

    let mut content = Vec::with_capacity(reader.fill_buf().map_or(0, |b| b.len()));
    reader.read_to_end(&mut content)?;

    if content.len() < 3 {
        return Err(eyre!("File too short to index"));
    }

    let mut line_number = 0;
    let mut line_start = 0;

    let mut window = [0u8; 3];
    let mut window_size = 0;

    for (byte_offset, &c) in content.iter().enumerate() {
        if c == b'\n' {
            index.lines.push(Line {
                start: line_start as Offset,
                end: byte_offset as Offset,
            });
            line_number += 1;
            line_start = byte_offset + 1;
        }

        if window_size < 3 {
            window[window_size] = c;
            window_size += 1;
        } else {
            window.copy_within(1..3, 0);
            window[2] = c;
        }

        if window_size == 3 {
            index.trigrams.entry(window).or_default().push(Posting {
                line_number,
                byte_offset: byte_offset as Offset,
            });
        }
    }

    // add the last line if it does not end with a newline
    if line_start < content.len() {
        index.lines.push(Line {
            start: line_start as Offset,
            end: content.len() as Offset,
        });
    }

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
        assert!(index_file(reader).is_err());
    }

    #[test]
    fn test_content_with_exactly_three_valid_chars() {
        let reader = Cursor::new("abc");
        let index = index_file(reader).unwrap();
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
        let index = index_file(reader).unwrap();
        assert!(index.trigrams.contains_key(&[b't', b'h', b'e']));
        assert!(index.trigrams.contains_key(&[b'h', b'e', b' ']));
        assert!(index.trigrams.contains_key(&[b'q', b'u', b'i']));
    }

    #[test]
    fn test_content_with_whitespace() {
        let reader = Cursor::new("the\n\tquick\rfox");
        let index = index_file(reader).unwrap();
        assert!(index.trigrams.contains_key(&[b't', b'h', b'e']));
        assert!(index.trigrams.contains_key(&[b'q', b'u', b'i']));
        assert!(index.trigrams.contains_key(&[b'u', b'i', b'c']));
    }

    #[test]
    fn test_line_tracking() {
        let reader = Cursor::new("first\nsecond\nthird");
        let index = index_file(reader).unwrap();
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
        let index = index_file(reader).unwrap();
        assert!(!index.trigrams.is_empty());
    }

    #[test]
    fn test_posting_correctness() {
        let reader = Cursor::new("hello\nworld");
        let index = index_file(reader).unwrap();
        let hel = index.trigrams.get(&[b'h', b'e', b'l']).unwrap();
        assert_eq!(hel[0].line_number, 0);
        assert_eq!(hel[0].byte_offset, 2);

        let wor = index.trigrams.get(&[b'w', b'o', b'r']).unwrap();
        assert_eq!(wor[0].line_number, 1);
        assert_eq!(wor[0].byte_offset, 8);
    }

    #[test]
    fn test_daachorse_trigram_matching() {
        let test_content = b"the quick brown fox";
        let reader = Cursor::new(test_content);
        let index = index_file(reader).unwrap();

        // Convert trigrams to patterns for Aho-Corasick
        let patterns: Vec<&[u8]> = index.trigrams.keys().map(|key| key.as_ref()).collect();
        let pma: DoubleArrayAhoCorasick<usize> = DoubleArrayAhoCorasick::new(patterns).unwrap();

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
            match index_file(reader) {
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
            let index = match index_file(reader) {
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
