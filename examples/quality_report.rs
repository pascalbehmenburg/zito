//! Compare verified match locations against a direct line-scan oracle.
use std::{
    collections::BTreeSet,
    fs,
    panic::{AssertUnwindSafe, catch_unwind},
    path::PathBuf,
};
use zito::{Index, IndexView, SearchOptions};

type Hit = (String, u32, u32, u32);
fn main() -> eyre::Result<()> {
    let root = PathBuf::from(
        std::env::args().nth(1).expect("fresh scratch directory"),
    );
    let corpus = root.join("corpus");
    fs::create_dir_all(&corpus)?;
    let corpus = corpus.canonicalize()?;
    let mut files = Vec::new();
    for n in 0..16 {
        let text = format!(
            "foo foo food {n:03}\r\nbar x café 🦀 日本語\nFOO aaaaa abcabc\n\nfn handle_{n:03}() {{}}\n"
        );
        let path = corpus.join(format!("{n}.rs"));
        fs::write(&path, &text)?;
        files.push((path.canonicalize()?.to_string_lossy().into_owned(), text));
    }
    let path = root.join("main.zito");
    #[allow(unused_mut)]
    let mut index = Index::new_from_path(&corpus)?;
    index.store(&path)?;
    let view = IndexView::try_from(&path)?;
    let mut total_tp = 0;
    let mut total_fp = 0;
    let mut total_fn = 0;
    let mut passed = 0;
    let mut queries = 0;
    let mut errors = 0;
    let mut duplicates = 0;
    // The baseline can panic on Unicode; count it as a failed query, then
    // continue collecting quality evidence for the remaining synthetic cases.
    std::panic::set_hook(Box::new(|_| {}));
    for (is_regex, patterns) in [
        (
            false,
            vec![
                "foo",
                "oo",
                "x",
                "é",
                "🦀",
                "123",
                "absent",
                "aaaa",
                "foo foo",
                "café",
                "handle_007",
                "日本語",
            ],
        ),
        (
            true,
            vec![
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
            ],
        ),
    ] {
        for query in patterns {
            queries += 1;
            let mut expected = BTreeSet::new();
            let re = if is_regex {
                Some(regex::Regex::new(query)?)
            } else {
                None
            };
            for (path, text) in &files {
                for (number, line) in text.lines().enumerate() {
                    if let Some(re) = &re {
                        for m in re.find_iter(line) {
                            expected.insert((
                                path.clone(),
                                number as u32,
                                m.start() as u32,
                                m.end() as u32,
                            ));
                        }
                    } else {
                        for (i, _) in line.char_indices() {
                            if line[i..].starts_with(query) {
                                expected.insert((
                                    path.clone(),
                                    number as u32,
                                    i as u32,
                                    (i + query.len()) as u32,
                                ));
                            }
                        }
                    }
                }
            }
            let actual = catch_unwind(AssertUnwindSafe(|| {
                view.search(query, SearchOptions::new(is_regex))
            }));
            let (actual, failed): (Vec<Hit>, bool) = match actual {
                Ok(Ok(hits)) => (
                    hits.into_iter()
                        .map(|r| {
                            (
                                r.file_path,
                                r.line_number,
                                r.match_start,
                                r.match_end,
                            )
                        })
                        .collect(),
                    false,
                ),
                _ => {
                    errors += 1;
                    (Vec::new(), true)
                }
            };
            let raw_count = actual.len();
            let actual: BTreeSet<_> = actual.into_iter().collect();
            duplicates += raw_count - actual.len();
            let tp = actual.intersection(&expected).count();
            let fp = actual.difference(&expected).count();
            let missed = expected.difference(&actual).count();
            total_tp += tp;
            total_fp += fp;
            total_fn += missed;
            if !failed && fp == 0 && missed == 0 && raw_count == actual.len() {
                passed += 1;
            }
            eprintln!(
                "QUALITY regex={is_regex} query={query:?} tp={tp} fp={fp} fn={missed} error={failed} duplicates={}",
                raw_count - actual.len()
            );
        }
    }
    eprintln!(
        "QUALITY_TOTAL queries={queries} passed={passed} errors={errors} duplicates={duplicates} tp={total_tp} fp={total_fp} fn={total_fn}"
    );
    Ok(())
}
