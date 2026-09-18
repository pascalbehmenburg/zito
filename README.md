# Zito

Fast local code search with an incremental sparse n-gram index.

```sh
cargo build --release
zito index create ./project ./index
zito find 'handle_request' ./project -i ./index
zito find 'foo|bar' ./project -i ./index --regex
```

`find` refreshes the selected directory before searching. Only new or changed
files are read and indexed; deletions and renames remove the old documents.
Use `--no-update` to search the existing snapshot without a filesystem scan.
Use `--verify` to read and compare every file instead of trusting metadata.

```sh
zito index update ./project ./index
zito index update ./project ./index --verify
zito index compact ./index
zito index merge ./other-index ./index
```

`index extend` remains an alias for `index update`. Updates synchronize only
the supplied directory; other indexed roots remain intact. Merges replace
same-path documents with the incoming version. `index create` explicitly
replaces an index and is the migration path from the old trigram format.

## Search semantics

- Searches are case-sensitive and line-oriented. Literal matches overlap;
  regex matches follow the `regex` crate's non-overlapping `find_iter` behavior.
- One- and two-byte queries work. Regexes without useful literal prefixes fall
  back to a full scan. Empty literals are rejected; empty regexes are supported.
- Results are verified against indexed contents and sorted by canonical path,
  line, and match position. The library uses zero-based line numbers and UTF-8
  **byte** offsets; the CLI displays one-based positions.
- UTF-8 text, including empty files, is indexed. Invalid UTF-8 and NUL-containing
  files are skipped. Symlinks are not followed. `.git` directories and Zito's
  index artifacts are excluded. Other hidden/ignored files are included;
  `.gitignore` rules are not interpreted. Non-UTF-8 paths and files over 4 GiB
  return an error rather than silently dropping matches.
- There is no relevance ranking, stemming, typo correction, or semantic search.
  Sparse n-grams are a candidate filter, not a relevance score.

## Incremental storage

`main.zito` is a small versioned manifest referencing immutable
`.zito-segment-*.zseg` files beside it. Keep those files together. Updates append
only changed documents and tombstones, then atomically publish the manifest.
A no-op update writes nothing. File locks and optimistic version checks reject
stale writers; reopen and retry if another process has updated the index.

Default change detection uses size and nanosecond mtime, plus ctime/device/inode
on Unix. `--verify` is the strict option for filesystems with unreliable metadata.
Scans are staged: a read/traversal error leaves the prior snapshot intact. Files
that change while being read cause an error and can be retried. This is not a
filesystem-wide transactional snapshot or a live watcher.

Run `index compact` to consolidate segments and reclaim superseded content.
Compaction is explicit; segment count and disk usage otherwise grow with edits.
Already-open readers remain valid. Interrupted publication may leave an
unreferenced segment; it is ignored. Indices contain local source snapshots,
including older versions until compaction.

Segments use checked, aligned rkyv archives and CRC32 checksums. They load into
owned buffers without rebuilding posting lists or an automaton. Uncompressed
storage favors latency over disk size; opening still reads and validates every
segment. See [design](docs/adr/0001-incremental-sparse-index.md) and
[performance and search-quality analysis](docs/EVALUATION.md).

## Library migration

`Index` exposes methods rather than the old public trigram/interner internals.
`store` takes `&mut self` and advances the committed snapshot. `merge` returns a
`Result`. Convert with `Index::from(view)`, update, store, and call `into_view()`
to search without reopening. Use `replace` for an explicit overwrite.

```rust,no_run
use zito::{Index, SearchOptions, UpdateOptions};

fn main() -> eyre::Result<()> {
    let mut index = Index::new();
    index.update_by_path("./project", UpdateOptions::default())?;
    index.store("./index/main.zito")?;
    let view = index.into_view()?;
    let matches = view.search("handle_request", SearchOptions::default())?;
    println!("{} matches", matches.len());
    Ok(())
}
```

## Development

Use stable Rust (`rust-toolchain.toml`) and the committed lockfile.

```sh
cargo fmt --check
QUICKCHECK_TESTS=20000 cargo test --locked
cargo clippy --all-targets --locked -- -D warnings
cargo build --release --locked
```

The byte-oriented n-gram implementation is adapted from
[pascalbehmenburg/sparse_ngram](https://github.com/pascalbehmenburg/sparse_ngram),
with attribution in [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md).
