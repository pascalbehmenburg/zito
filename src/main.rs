use clap::{Parser, Subcommand};
use colored::Colorize;
use eyre::Result;
use std::{path::PathBuf, time::Instant};
use zito::{Index, IndexView, SearchOptions, UpdateOptions};

#[derive(Parser)]
#[command(name = "zito", about = "Fast incremental sparse n-gram code search")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Refresh changed files, then search the indexed snapshot.
    Find {
        query: String,
        #[arg(default_value = "./")]
        search_dir: PathBuf,
        #[arg(short, long, default_value = "./")]
        index_dir: PathBuf,
        #[arg(short, long)]
        regex: bool,
        /// Search the existing snapshot without scanning file metadata.
        #[arg(long)]
        no_update: bool,
        /// Compare file contents even when timestamps and sizes are unchanged.
        #[arg(long, conflicts_with = "no_update")]
        verify: bool,
    },
    Index {
        #[command(subcommand)]
        command: IndexCommands,
    },
}

#[derive(Subcommand)]
enum IndexCommands {
    /// Build a new index (also replaces legacy indices).
    Create {
        search_dir: PathBuf,
        #[arg(default_value = "./")]
        index_dir: PathBuf,
    },
    /// Incrementally synchronize additions, changes, renames and deletions.
    #[command(alias = "extend")]
    Update {
        search_dir: PathBuf,
        #[arg(default_value = "./")]
        index_dir: PathBuf,
        #[arg(long)]
        verify: bool,
    },
    Merge {
        other_index_dir: PathBuf,
        #[arg(default_value = "./")]
        index_dir: PathBuf,
    },
    /// Consolidate segments and reclaim superseded content.
    Compact {
        #[arg(default_value = "./")]
        index_dir: PathBuf,
    },
}

fn main() -> Result<()> {
    match Cli::parse().command {
        Commands::Find {
            query,
            search_dir,
            index_dir,
            regex,
            no_update,
            verify,
        } => {
            let path = index_dir.join("main.zito");
            let root = search_dir.canonicalize()?;
            let view = if no_update {
                IndexView::try_from(&path)?
            } else {
                let mut index = if path.try_exists()? {
                    Index::from(IndexView::try_from(&path)?)
                } else {
                    Index::new()
                };
                index.update_by_path(
                    &root,
                    UpdateOptions {
                        verify_contents: verify,
                    },
                )?;
                index.store(&path)?;
                index.into_view()?
            };
            let start = Instant::now();
            let results = view.search(&query, SearchOptions::new(regex))?;
            let results: Vec<_> = results
                .into_iter()
                .filter(|r| {
                    std::path::Path::new(&r.file_path).starts_with(&root)
                })
                .collect();
            eprintln!(
                "Found {} matches in {} microseconds.",
                results.len(),
                start.elapsed().as_micros()
            );
            for result in results {
                let a = result.match_start as usize;
                let b = result.match_end as usize;
                println!(
                    "{}:{}:{}:\t{}{}{}",
                    result.file_path.blue(),
                    result.line_number + 1,
                    a + 1,
                    &result.line_text[..a],
                    result.line_text[a..b].blue().bold(),
                    &result.line_text[b..]
                );
            }
        }
        Commands::Index { command } => match command {
            IndexCommands::Create {
                search_dir,
                index_dir,
            } => Index::new_from_path(search_dir)?
                .replace(index_dir.join("main.zito"))?,
            IndexCommands::Update {
                search_dir,
                index_dir,
                verify,
            } => {
                let path = index_dir.join("main.zito");
                let mut index = Index::from(IndexView::try_from(&path)?);
                let stats = index.update_by_path(
                    search_dir,
                    UpdateOptions {
                        verify_contents: verify,
                    },
                )?;
                index.store(&path)?;
                eprintln!(
                    "added={} modified={} removed={} unchanged={} skipped={} bytes_read={}",
                    stats.added,
                    stats.modified,
                    stats.removed,
                    stats.unchanged,
                    stats.skipped,
                    stats.bytes_read
                );
            }
            IndexCommands::Merge {
                other_index_dir,
                index_dir,
            } => {
                let path = index_dir.join("main.zito");
                let mut index = Index::from(IndexView::try_from(&path)?);
                index
                    .merge(Index::from(IndexView::try_from(
                        &other_index_dir.join("main.zito"),
                    )?))?
                    .store(&path)?;
            }
            IndexCommands::Compact { index_dir } => {
                let path = index_dir.join("main.zito");
                Index::from(IndexView::try_from(&path)?).compact(&path)?;
            }
        },
    }
    Ok(())
}
