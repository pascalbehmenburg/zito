use std::{fs, path::PathBuf};
use tempfile::TempDir;
use zito::{Index, IndexView, SearchOptions};

fn fixture(content: &str) -> (TempDir, IndexView) {
    let temp = TempDir::new().unwrap();
    let corpus = temp.path().join("corpus");
    fs::create_dir(&corpus).unwrap();
    fs::write(corpus.join("sample.rs"), content).unwrap();
    let path = temp.path().join("main.zito");
    Index::new_from_path(corpus).unwrap().store(&path).unwrap();
    let view = IndexView::try_from(&path).unwrap();
    (temp, view)
}

#[test]
fn every_unicode_substring_agrees_with_a_line_scan() {
    let content =
        "café café 🦀🦀 日本語\r\nnaïve e\u{301} abcabc aaaaa\n\n末尾";
    let (_temp, view) = fixture(content);
    let mut boundaries: Vec<_> =
        content.char_indices().map(|(i, _)| i).collect();
    boundaries.push(content.len());
    for (a, &start) in boundaries.iter().enumerate() {
        for &end in &boundaries[a + 1..] {
            let query = &content[start..end];
            let mut expected = Vec::new();
            for (line_number, line) in content.lines().enumerate() {
                for (offset, _) in line.char_indices() {
                    if line[offset..].starts_with(query) {
                        expected.push((
                            line_number as u32,
                            offset as u32,
                            (offset + query.len()) as u32,
                        ));
                    }
                }
            }
            let found = view.search(query, SearchOptions::default()).unwrap();
            let actual: Vec<_> = found
                .iter()
                .map(|r| (r.line_number, r.match_start, r.match_end))
                .collect();
            assert_eq!(actual, expected, "query {query:?}");
            for result in found {
                assert_eq!(
                    &result.line_text[result.match_start as usize
                        ..result.match_end as usize],
                    query
                );
            }
        }
    }
    assert!(view.search("", SearchOptions::default()).is_err());
}

#[test]
fn regexes_agree_with_regex_crate_on_every_line() {
    let content =
        "foo foo food\r\nbar 123 baz x\nFOO café 🦀 日本語\n\nabcabc\r";
    let (_temp, view) = fixture(content);
    for query in [
        "foo",
        "foo|x",
        "foo|",
        "f(?:oo)?",
        "^",
        "$",
        ".*",
        "",
        r"\d+",
        r"[[:alpha:]]+",
        "(?i)foo",
        "foo.*food",
        "foo|bar",
        "(?:ab){2,4}",
        r"\bfoo\b",
        "café|日本語",
        "🦀",
        "(?i)cafÉ",
        r"[^a-z]{1,3}",
        "f.*?o",
        "x?",
        "(?m)^foo",
        "(?s:foo.*bar)",
    ] {
        let re = regex::Regex::new(query).unwrap();
        let expected: Vec<_> = content
            .lines()
            .enumerate()
            .flat_map(|(line_number, line)| {
                re.find_iter(line).map(move |m| {
                    (line_number as u32, m.start() as u32, m.end() as u32)
                })
            })
            .collect();
        let actual: Vec<_> = view
            .search(query, SearchOptions::new(true))
            .unwrap()
            .iter()
            .map(|r| (r.line_number, r.match_start, r.match_end))
            .collect();
        assert_eq!(actual, expected, "regex {query:?}");
    }
    assert!(view.search("[", SearchOptions::new(true)).is_err());
}

#[test]
fn empty_index_and_tiny_files_are_searchable() {
    let (_temp, view) = fixture("");
    assert_eq!(view.document_count(), 1);
    assert!(
        view.search("abc", SearchOptions::default())
            .unwrap()
            .is_empty()
    );
    assert!(
        view.search(".*", SearchOptions::new(true))
            .unwrap()
            .is_empty()
    );
    let (_temp, view) = fixture("x");
    assert_eq!(view.search("x", SearchOptions::default()).unwrap().len(), 1);
    let (_temp, view) = fixture("é");
    assert_eq!(view.search("é", SearchOptions::default()).unwrap().len(), 1);
    let temp = TempDir::new().unwrap();
    let path: PathBuf = temp.path().join("empty.zito");
    Index::new().store(&path).unwrap();
    assert!(
        IndexView::try_from(&path)
            .unwrap()
            .search("x", SearchOptions::default())
            .unwrap()
            .is_empty()
    );
}

#[test]
fn generated_ascii_corpus_has_exact_precision_and_recall() {
    let mut state = 0x12345678u64;
    let alphabet = b"abcdef012345_ \n";
    let mut content = String::new();
    for _ in 0..3000 {
        state = state.wrapping_mul(6364136223846793005).wrapping_add(1);
        content.push(alphabet[(state >> 32) as usize % alphabet.len()] as char);
    }
    let (_temp, view) = fixture(&content);
    for start in (0..content.len() - 30).step_by(7) {
        for len in [1, 2, 3, 7, 16, 29] {
            let query = &content[start..start + len];
            let expected: Vec<_> = content
                .lines()
                .enumerate()
                .flat_map(|(line, text)| {
                    (0..text.len())
                        .filter(move |&i| text[i..].starts_with(query))
                        .map(move |i| (line as u32, i as u32))
                })
                .collect();
            let actual: Vec<_> = view
                .search(query, SearchOptions::default())
                .unwrap()
                .iter()
                .map(|r| (r.line_number, r.match_start))
                .collect();
            assert_eq!(actual, expected, "query {query:?}");
        }
    }
}
