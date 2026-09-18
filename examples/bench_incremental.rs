//! Measure accumulated segment cost and compaction on synthetic edits.
use std::{fs, hint::black_box, time::Instant};
use zito::{Index, IndexView, SearchOptions};

fn main() -> eyre::Result<()> {
    let temp = tempfile::tempdir()?;
    let corpus = temp.path().join("corpus");
    fs::create_dir(&corpus)?;
    for file in 0..64 {
        fs::write(
            corpus.join(format!("{file}.rs")),
            format!("fn marker_{file:03}() {{}}\n").repeat(32),
        )?;
    }
    let path = temp.path().join("main.zito");
    let mut index = Index::new_from_path(&corpus)?;
    index.store(&path)?;
    let mut rows = Vec::new();
    for edit in 0..64 {
        if edit > 0 {
            fs::write(
                corpus.join("0.rs"),
                format!("fn marker_{edit:03}() {{}}\n").repeat(32),
            )?;
            index.extend_by_path(&corpus)?.store(&path)?;
        }
        if [0, 7, 15, 31, 63].contains(&edit) {
            rows.push(measure(&path, "before_compaction")?);
        }
    }
    let start = Instant::now();
    index.compact(&path)?;
    let compact_us = start.elapsed().as_micros();
    rows.push(measure(&path, "after_compaction")?);
    println!(
        "{}",
        serde_json::to_string_pretty(
            &serde_json::json!({"compaction_us": compact_us, "observations": rows})
        )?
    );
    Ok(())
}

fn measure(
    path: &std::path::Path,
    phase: &str,
) -> eyre::Result<serde_json::Value> {
    let start = Instant::now();
    let view = IndexView::try_from(path)?;
    let open_us = start.elapsed().as_micros();
    let mut timings = Vec::new();
    for _ in 0..101 {
        let start = Instant::now();
        black_box(view.search("marker_063", SearchOptions::default())?);
        timings.push(start.elapsed().as_nanos());
    }
    timings.sort_unstable();
    let (hits, stats) =
        view.search_with_stats("marker_063", SearchOptions::default())?;
    let bytes: u64 = fs::read_dir(path.parent().unwrap())?
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|s| s == "zseg" || s == "zito"))
        .map(|p| fs::metadata(p).unwrap().len())
        .sum();
    Ok(
        serde_json::json!({"phase":phase, "segments":view.segment_count(), "open_us":open_us, "query_median_ns":timings[50], "stored_bytes":bytes, "candidate_documents":stats.candidate_documents, "posting_lookups":stats.posting_lookups, "matches":hits.len()}),
    )
}
