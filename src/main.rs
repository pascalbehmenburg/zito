use colored::*;
use eyre::Result;
use std::{
    env::current_dir,
    time::{Duration, SystemTime},
};
use zito::{Index, IndexView, SearchOptions};

/// Time the execution of a function and return the result and the duration in milliseconds.
fn timeit<F: Fn() -> T, T>(f: F) -> (T, Duration) {
    let start = SystemTime::now();
    let result = f();
    let end = SystemTime::now();
    let duration = end.duration_since(start).unwrap();
    (result, duration)
}

fn main() -> Result<()> {
    println!("Creating index from src directory...");
    let (index, duration) = timeit(|| Index::new_from_path("./src"));
    let index = index?;
    println!("Index created in {} microseconds", duration.as_micros());
    println!();

    index.interned_paths.print_detailed_stats();

    let index_dir = current_dir()?.join("index");
    std::fs::create_dir_all(&index_dir)?;
    let index_path = index_dir.join("main.zito");
    index.store(&index_path)?;
    println!("Index stored to {}", index_path.display());

    let index_view = IndexView::try_from(index_path.as_path())?;
    println!("Index loaded successfully");

    let queries = vec!["fn main", "use", "struct", "impl"];

    println!(
        "\nSearching for: '{}'",
        r#"println!\s*\(\s*(.*?)\s*\)"#.red().bold()
    );

    let (results, duration) = timeit(|| {
        index_view
            .search(r#"println!\s*\(\s*(.*?)\s*\)"#, SearchOptions::new(true))
    });
    let results = results?;

    // if results.is_empty() {
    // println!("\tNo matches found");
    // continue;
    // }

    println!();
    println!(
        "Found {} matches in {} microseconds:",
        results.len(),
        duration.as_micros()
    );

    // group results by file and sort by line number
    let mut file_results: std::collections::HashMap<String, Vec<_>> =
        std::collections::HashMap::new();

    for result in results {
        file_results
            .entry(result.file_path.clone())
            .or_insert_with(Vec::new)
            .push(result);
    }

    let mut sorted_files: Vec<_> = file_results.into_iter().collect();
    sorted_files.sort_by(|a, b| a.0.cmp(&b.0));

    for (file_path, mut file_matches) in sorted_files {
        file_matches.sort_by_key(|r| r.line_number);

        for result in file_matches.iter().take(5) {
            // limit to first 5 matches per file
            println!(
                "{}:{}:{}: {}",
                file_path.green(),
                result.line_number.to_string().yellow(),
                result.match_start.to_string().blue(),
                highlight_match(
                    &result.line_text,
                    result.match_start as usize,
                    result.match_end as usize
                )
            );
        }
    }

    std::fs::remove_file(&index_path).ok();
    std::fs::remove_dir(&index_dir).ok();

    Ok(())
}

fn highlight_match(line: &str, match_start: usize, match_end: usize) -> String {
    let mut result = String::new();
    result.push_str(&line[..match_start]);
    result.push_str(&line[match_start..match_end].red().bold().to_string());
    result.push_str(&line[match_end..]);
    result
}
