use crate::Commands::Find;
use crate::IndexCommands::{Create, Extend, Merge};
use clap::{Parser, Subcommand};
use colored::*;
use eyre::Result;
use std::{
    path::{Path, PathBuf},
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
        search_dir: PathBuf,

        /// In which folder is or will the index.zito file be stored
        /// Make sure it is the index that was used to index the search_loc.
        #[arg(short, long, default_value = "./")]
        index_dir: PathBuf,

        /// Whether to interpret the query as a regex
        #[arg(short, long, default_value_t = false)]
        regex: bool,
    },
    /// Index management commands
    Index {
        #[command(subcommand)]
        command: IndexCommands,
    },
}

#[derive(Debug, Subcommand)]
enum IndexCommands {
    Create {
        /// The directory to index
        search_dir: PathBuf,

        /// The directory to store the index in
        #[arg(default_value = "./")]
        index_dir: PathBuf,
    },
    /// Extend an index by merging another index into it
    Merge {
        /// The index to merge from
        other_index_dir: PathBuf,

        /// The index to merge into
        #[arg(default_value = "./")]
        index_dir: PathBuf,
    },
    Extend {
        /// The index to extend from
        search_dir: PathBuf,

        /// The index to extend
        #[arg(default_value = "./")]
        index_dir: PathBuf,
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
    let index_file_name = "main.zito";
    match args.command {
        Find {
            query,
            search_dir,
            index_dir,
            regex,
        } => {
            let index_path = index_dir.join(index_file_name);
            // check whether index_loc already contains an index
            let index_view = match IndexView::try_from(index_path.as_path()) {
                Ok(index) => index,
                Err(_) => {
                    let (index_result, index_duration) = timeit(|| {
                        std::fs::create_dir_all(&index_dir)?;
                        Index::new_from_path(search_dir.as_path())
                            .and_then(|index| index.store(index_path.as_path()))
                    });
                    index_result?;
                    println!(
                        "Created and stored index in {} seconds.",
                        index_duration.as_secs().to_string().blue()
                    );
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

            // sort files by name and filter out files which are outside of the search location
            let mut sorted_files: Vec<_> = file_results
                .into_iter()
                .filter(|(path, _)| {
                    let file_path = Path::new(path).canonicalize().unwrap();
                    let folder_path = search_dir.canonicalize().unwrap();
                    file_path.starts_with(folder_path)
                })
                .collect();
            sorted_files.sort_by(|a, b| a.0.cmp(&b.0));

            for (file_path, mut file_matches) in sorted_files {
                file_matches.sort_by_key(|r| r.line_number);

                println!();
                for result in file_matches.iter() {
                    println!(
                        "{}:{}:{}:\t{}",
                        file_path.blue(),
                        // line starts are offset by 1
                        (result.line_number + 1),
                        // a trigram is 3 chars long so we need to add 3
                        (result.match_start + 1),
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
        Commands::Index { command } => match command {
            Create {
                search_dir,
                index_dir,
            } => {
                let (index_result, index_duration) = timeit(|| {
                    let index_path = index_dir.join(index_file_name);
                    std::fs::create_dir_all(&index_dir)?;
                    Index::new_from_path(search_dir.as_path())?
                        .store(index_path.as_path())
                });
                index_result?;
                println!(
                    "Created and stored index in {} seconds.",
                    index_duration.as_secs().to_string().blue()
                );
            }
            Merge {
                other_index_dir,
                index_dir,
            } => {
                let (index_result, index_duration) = timeit(|| {
                    let index_path = index_dir.join(index_file_name);

                    let other_index = Index::try_from(IndexView::try_from(
                        &other_index_dir.join(index_file_name),
                    )?)?;

                    Index::try_from(IndexView::try_from(&index_path)?)?
                        .merge(other_index)
                        .store(index_path)
                });
                index_result?;
                println!(
                    "Merged index in {} seconds.",
                    index_duration.as_secs().to_string().blue()
                );
            }
            Extend {
                search_dir,
                index_dir,
            } => {
                let (index_result, index_duration) = timeit(|| {
                    let index_path = index_dir.join(index_file_name);
                    Index::try_from(IndexView::try_from(&index_path)?)?
                        .extend_by_path(&search_dir)?
                        .store(index_path)
                });
                index_result?;
                println!(
                    "Extended index in {} seconds.",
                    index_duration.as_secs().to_string().blue()
                );
            }
        },
    }
    Ok(())
}
