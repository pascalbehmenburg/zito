//! Deterministic synthetic workload shared with the unmodified baseline.
use std::{fs, hint::black_box, path::PathBuf, time::Instant};
use zito::{Index, IndexView, SearchOptions};

fn main() -> eyre::Result<()> {
    let root =
        PathBuf::from(std::env::args().nth(1).expect("scratch directory"));
    let files: usize =
        std::env::args().nth(2).unwrap_or("256".into()).parse()?;
    let mixed = std::env::args().nth(3).as_deref() == Some("mixed");
    let corpus = root.join("corpus");
    fs::create_dir_all(&corpus)?;
    for file in 0..files {
        let mut content = String::new();
        for line in 0..64 {
            if mixed {
                let hash = (file as u64 * 64 + line)
                    .wrapping_mul(6364136223846793005)
                    .wrapping_add(1442695040888963407);
                content.push_str(&format!(
                    "// payload_{hash:016x} key_{:016x}\n",
                    hash.rotate_left(23)
                ));
            }
            content.push_str(&format!(
                "pub fn handle_{file:05}_{line:03}(request: Request) -> Result<Response> {{ dispatch(request, {file}); }}\n"
            ));
        }
        fs::write(corpus.join(format!("source_{file:05}.rs")), content)?;
    }
    let path = root.join("main.zito");
    let start = Instant::now();
    let mut index = Index::new_from_path(&corpus)?;
    eprintln!("METRIC build_us {}", start.elapsed().as_micros());
    let start = Instant::now();
    index.store(&path)?;
    eprintln!("METRIC store_us {}", start.elapsed().as_micros());
    let stored_bytes: u64 = fs::read_dir(&root)?
        .map(|entry| entry.unwrap().path())
        .filter(|p| {
            p.extension()
                .is_some_and(|ext| ext == "zito" || ext == "zseg")
        })
        .map(|p| fs::metadata(p).unwrap().len())
        .sum();
    eprintln!("METRIC stored_bytes {stored_bytes}");
    let start = Instant::now();
    let view = IndexView::try_from(&path)?;
    eprintln!("METRIC open_us {}", start.elapsed().as_micros());
    for query in [
        "handle_00127_032",
        "dispatch",
        "handle_99999_999",
        "Result<Response>",
    ] {
        let mut samples = Vec::new();
        let mut count = 0;
        for _ in 0..21 {
            let start = Instant::now();
            count =
                black_box(view.search(query, SearchOptions::default())?).len();
            samples.push(start.elapsed().as_nanos());
        }
        samples.sort_unstable();
        eprintln!(
            "METRIC query {query} median_ns {} p95_ns {} matches {count}",
            samples[10], samples[19]
        );
    }
    let mut index = Index::from(view);
    let start = Instant::now();
    index.extend_by_path(&corpus)?.store(&path)?;
    eprintln!(
        "METRIC noop_update_store_us {}",
        start.elapsed().as_micros()
    );
    fs::write(
        corpus.join("source_00000.rs"),
        "pub fn replacement_marker() {}\n",
    )?;
    let start = Instant::now();
    index.extend_by_path(&corpus)?.store(&path)?;
    eprintln!(
        "METRIC one_file_update_store_us {}",
        start.elapsed().as_micros()
    );
    Ok(())
}
