//! Incremental sparse n-gram code search. Postings filter documents; literal
//! and regex matchers verify every result against the indexed UTF-8 snapshot.
mod index;
mod ngram;
mod search;
mod storage;

pub use index::{Index, IndexView, UpdateOptions, UpdateStats};
pub use search::SearchStats;

/// A verified, line-oriented match. Offsets are UTF-8 byte offsets (0-based),
/// suitable for slicing `line_text`; line numbers are also 0-based.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SearchResult {
    pub file_path: String,
    pub line_number: u32,
    pub line_text: String,
    pub match_start: u32,
    pub match_end: u32,
}

#[derive(Clone, Copy, Default)]
pub struct SearchOptions {
    pub regex: bool,
}

impl SearchOptions {
    pub fn new(regex: bool) -> Self {
        Self { regex }
    }
}
