use criterion::{Criterion, black_box, criterion_group, criterion_main};
use daachorse::DoubleArrayAhoCorasick;
use std::{collections::HashMap, io::Cursor, path::PathBuf};
use zito::{FileId, Offset, Trigram, index_file};

fn bench_index_file(c: &mut Criterion) {
    let content = include_str!("data.txt");
    let data = content.as_bytes();

    c.bench_function("index_file", |b| {
        b.iter(|| {
            let reader = Cursor::new(black_box(data));
            black_box(index_file(reader, PathBuf::new()).unwrap());
        });
    });
}

fn bench_search(c: &mut Criterion) {
    let sample_text = include_str!("data.txt");
    let reader = Cursor::new(sample_text.as_bytes());
    let index = index_file(reader, PathBuf::new()).unwrap();

    // Convert the new index format to the format needed for Aho-Corasick
    let mut trigram_index: HashMap<Trigram, Vec<(FileId, Offset)>> =
        HashMap::new();
    for (trigram, postings) in index.trigrams {
        let entries = trigram_index.entry(trigram).or_default();
        for posting in postings.as_slice() {
            entries.push((0, posting.byte_offset)) // Using 0 as file_id since we have only one file
        }
    }

    let patterns: Vec<&[u8]> =
        trigram_index.keys().map(|key| key.as_ref()).collect();
    let pma: DoubleArrayAhoCorasick<usize> =
        DoubleArrayAhoCorasick::new(patterns).unwrap();

    c.bench_function("search_many_occurences", |b| {
        b.iter(|| {
            black_box(pma.find_overlapping_iter(black_box(b"test")).count())
        })
    });

    c.bench_function("search_less_occurences", |b| {
        b.iter(|| {
            black_box(
                pma.find_overlapping_iter(black_box(b"def main()")).count(),
            )
        })
    });

    c.bench_function("search_first_occurence", |b| {
        b.iter(|| {
            black_box(pma.find_overlapping_iter(black_box(b"arxiv")).count())
        });
    });

    c.bench_function("search_pattern", |b| {
        b.iter(|| {
            let pattern = black_box(b"function");
            black_box(pma.find_overlapping_iter(pattern).count())
        });
    });
}

criterion_group!(benches, bench_index_file, bench_search);
criterion_main!(benches);
