use crate::Commands::Find;
use clap::{Parser, Subcommand};
use colored::*;
use eyre::Result;
use std::{
    path::PathBuf,
    time::{Duration, SystemTime},
};
use zito::{Index, IndexView, SearchOptions};

/// A code search cli
#[derive(Debug, Parser)]
#[command(name = "zito")]
#[command(about = "Does find code, fast.", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Find code
    #[command(arg_required_else_help = true)]
    Find {
        /// The query to search for
        query: String,

        /// Which folder to search in
        #[arg(default_value = "./")]
        search_loc: PathBuf,

        /// In which folder is or will the index.zito file be stored
        /// Make sure it is the index that was used to index the search_loc.
        #[arg(short, long, default_value = "./")]
        index_dir: PathBuf,

        /// Whether to interpret the query as a regex
        #[arg(short, long, default_value_t = false)]
        regex: bool,
    },
}

/// Time the execution of a function and return the result and the duration in milliseconds.
fn timeit<F: Fn() -> T, T>(f: F) -> (T, Duration) {
    let start = SystemTime::now();
    let result = f();
    let end = SystemTime::now();
    let duration = end.duration_since(start).unwrap();
    (result, duration)
}

fn highlight_match(line: &str, match_start: usize, match_end: usize) -> String {
    let mut result = String::new();
    result.push_str(&line[..match_start]);
    result.push_str(&line[match_start..match_end].blue().bold().to_string());
    result.push_str(&line[match_end..]);
    result
}

fn main() -> Result<()> {
    let args = Cli::parse();
    match args.command {
        Find {
            query,
            search_loc,
            index_dir,
            regex,
        } => {
            // check whether index_loc already contains an index
            let index_view = match IndexView::try_from(index_dir.as_path()) {
                Ok(index) => index,
                Err(_) => {
                    let index = Index::new_from_path(search_loc.as_path())?;

                    std::fs::create_dir_all(&index_dir)?;
                    let index_path = index_dir.join("main.zito");
                    index.store(&index_path)?;

                    IndexView::try_from(index_path.as_path())?
                }
            };

            let (results, duration) = timeit(|| {
                index_view.search(query.as_str(), SearchOptions::new(regex))
            });
            let results = results?;

            if results.is_empty() {
                println!("{}", "\tNo matches found.".red());
                return Ok(());
            }

            println!(
                "Found {} matches in {} microseconds.",
                results.len().to_string().blue(),
                duration.as_micros().to_string().blue()
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
                println!();
                for result in file_matches.iter() {
                    println!(
                        "{}:{}:{}:\t{}",
                        file_path.strip_prefix("/").unwrap_or_default().blue(),
                        (result.line_number + 1).to_string(),
                        result.match_start.to_string(),
                        highlight_match(
                            &result.line_text,
                            result.match_start as usize,
                            result.match_end as usize
                        )
                        .trim()
                    );
                }
            }
        }
    }
    Ok(())
}
