use colored::*;
use daachorse::DoubleArrayAhoCorasick;
use std::{collections::HashMap, env::args, env::current_dir, path::PathBuf};
use zito::{IndexView, Line, extract_symbols, index_files};

fn main() -> eyre::Result<()> {
    let args: Vec<String> = args().collect();

    let file_path = "./src/lib.rs";
    println!("Parsing file: {}", file_path);
    let lang = tree_sitter_rust::LANGUAGE;
    let query_scm = "./queries/rust.scm";
    let source_code = std::fs::read_to_string(file_path)?;
    let symbols = extract_symbols(&source_code, lang.into(), query_scm);
    for symbol in symbols {
        println!(
            "Name: {}, Kind: {}, Range: {:?}",
            symbol.name, symbol.kind, symbol.byte_range
        );
    }

    // example: index files in (e.g. src) dir
    let indices = index_files("src")?;

    // example: create index cache directory
    let index_dir = current_dir()?.join("index");
    std::fs::create_dir_all(&index_dir)?;
    for index in indices.iter() {
        let index_path = index_dir.join(
            Into::<PathBuf>::into(index.file_path.clone())
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string()
                + ".zito",
        );
        index.store(index_path)?;
    }

    // example: use walkdir to load index files
    let mut indices: Vec<IndexView> = Vec::new();
    walkdir::WalkDir::new("index")
        .into_iter()
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension()?.to_str()? == "zito" {
                Some(path.to_path_buf())
            } else {
                None
            }
        })
        .for_each(|path| {
            if let Ok(index) = TryInto::<IndexView>::try_into(path.as_path()) {
                indices.push(index);
                println!("Loaded index from {}", path.display());
            }
        });

    // example: use Aho-Corasick to search for a query
    for index in indices.iter() {
        let trigram_index = index.trigrams.iter();

        let daac = DoubleArrayAhoCorasick::with_values(trigram_index).map_err(
            |e| eyre::eyre!("Failed to build Aho-Corasick automaton: {}", e),
        )?;

        let content = std::fs::read_to_string(PathBuf::from(
            index.file_path.to_string(),
        ))?;

        let query = "fn main()";
        let mut matches_by_line: HashMap<(usize, String), Vec<usize>> =
            HashMap::new();

        for m in daac.find_iter(query) {
            let postings = m.value();
            for posting in postings {
                let line_number_idx: usize =
                    posting.line_number.to_native() as usize;
                let line_info: &rkyv::Archived<Line> =
                    index.lines.get(line_number_idx).unwrap();
                let line_content =
                    &content[usize::try_from(line_info.start.to_native())?
                        ..usize::try_from(line_info.end.to_native())?];

                if let Some(query_pos) = line_content.find(query) {
                    matches_by_line
                        .entry((
                            Into::<u32>::into(posting.line_number) as usize,
                            line_content.to_string(),
                        ))
                        .or_default()
                        .push(query_pos);
                }
            }
        }

        let mut matches: Vec<_> = matches_by_line.into_iter().collect();
        matches.sort_by_key(|((line_num, _), _)| *line_num);

        for ((line_number, line_content), mut positions) in matches {
            positions.sort_unstable();

            let mut merged_positions = Vec::new();
            let mut current_group = Vec::new();

            for &pos in positions.iter() {
                if current_group.is_empty()
                    || pos - current_group.last().unwrap() <= 3
                {
                    current_group.push(pos);
                } else {
                    if let Some(start_pos) = current_group.first() {
                        merged_positions
                            .push((*start_pos, *start_pos + query.len()));
                    }
                    current_group.clear();
                    current_group.push(pos);
                }
            }
            if let Some(start_pos) = current_group.first() {
                merged_positions.push((*start_pos, *start_pos + query.len()));
            }

            if !merged_positions.is_empty() {
                println!(
                    "{}:{}:{}: {}",
                    index.file_path,
                    line_number,
                    merged_positions[0].0,
                    highlight_matches(&line_content, &merged_positions)
                );
            }
        }
    }

    Ok(())
}

fn highlight_matches(line: &str, ranges: &[(usize, usize)]) -> String {
    let mut result = String::with_capacity(line.len());
    let mut pos = 0;

    for &(start, end) in ranges {
        result.push_str(&line[pos..start]);
        result.push_str(&line[start..end.min(line.len())].red().to_string());
        pos = end;
    }

    result.push_str(&line[pos..]);
    result
}
