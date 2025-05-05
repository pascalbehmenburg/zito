use colored::*;
use daachorse::DoubleArrayAhoCorasick;
use std::collections::HashMap;
use zito::index_files;

fn main() -> eyre::Result<()> {
    let indices = index_files("src")?;

    for index in indices.iter() {
        let trigram_index = index
            .trigrams
            .iter()
            .map(|(trigram, postings)| (trigram, postings.as_slice()));

        let daac = DoubleArrayAhoCorasick::with_values(trigram_index)
            .map_err(|e| eyre::eyre!("Failed to build Aho-Corasick automaton: {}", e))?;

        let content = std::fs::read_to_string(&index.file_path)?;

        let query = "fn main()";
        let mut matches_by_line: HashMap<(usize, String), Vec<usize>> = HashMap::new();

        for m in daac.find_iter(query) {
            let postings = m.value();

            for posting in postings {
                let line_info = index.lines.get(posting.line_number as usize).unwrap();
                let line_content = &content[line_info.start as usize..line_info.end as usize];

                if let Some(query_pos) = line_content.find(query) {
                    matches_by_line
                        .entry((posting.line_number as usize, line_content.to_string()))
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
                if current_group.is_empty() || pos - current_group.last().unwrap() <= 3 {
                    current_group.push(pos);
                } else {
                    if let Some(start_pos) = current_group.first() {
                        merged_positions.push((*start_pos, *start_pos + query.len()));
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
                    index.file_path.display(),
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
