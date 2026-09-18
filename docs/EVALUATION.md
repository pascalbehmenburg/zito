# Performance and search quality

Measured 2026-09-17 on **macOS / Apple M3 / 16 GiB RAM**, stable Rust 1.98.1,
release optimization, thin LTO, one codegen unit. Baseline:
`32f824261b2f425e9fb5d390040e15954625c36b`. Both revisions used the same stable
compiler and equivalent benchmark harnesses. The baseline retains its original
lockfile and allocator; the candidate updates dependencies and removes the
custom allocator. Results measure the complete implementations, not the isolated
causal effect of sparse grams.

The default Xcode selection failed locally. Builds used
`DEVELOPER_DIR=/Library/Developer/CommandLineTools` and
`SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX15.4.sdk` without changing
system configuration. Linux and Windows were not run.

## Method and recorded attempts

The implementation follows a small measurement-and-selection loop inspired by
[Dream-RSI](https://arxiv.org/html/2609.14858v1): define correctness gates, record
attempts, compare outcomes, keep changes supported by measurements. It does not
implement the paper's recursive policy-development agent or discovery-tree
simulator. No end-to-end token savings were measured or inferred.

[Attempt ledger](benchmarks/attempts.jsonl) records an initial sorted-vector gram
deduplication implementation and its hash-set replacement. Preliminary single
runs reduced generation/persistence time from 60.5 to 49.1 ms on repetitive
input, and 594.8 to 473.9 ms on mixed input. Those exploratory runs are not a
confidence estimate. The selected version additionally bounds the monotonic
anchor workspace and reuses the committed view in the CLI. Only the final
comparison below has three complete repetitions.

Two deterministic synthetic corpora exercise different gram distributions:

- **Repetitive:** 256 files, 64 source lines each; shared function structure and
  per-file/per-line identifiers (16,384 source lines).
- **Mixed:** 1,024 files with the same structure plus deterministic varying
  hexadecimal payload comments (65,536 source lines plus comments).

Each complete run generates a fresh corpus, builds/stores/opens its index, runs
21 searches per query, refreshes without changes, then replaces one file and
refreshes again. Variants execute sequentially and alternate per repetition.
Reported operation times are medians of three runs; query times are medians of
the three per-run medians. Per-run p95 values and all observations are saved in
[raw measurements](benchmarks/m3-2026-09-17.json).

Filesystem caches were not flushed: **these are warm-cache measurements**.
Refresh numbers start with an already-open index and include persistence, not
process startup or opening the old snapshot. Build+store includes gram
construction even though the new implementation defers it to store. Query
numbers include result allocation/sorting but exclude CLI output and refresh.
The baseline's library prints one diagnostic line per query; stdout was captured.

## Axis 1: performance

| Operation | Repetitive baseline | Repetitive sparse | Mixed baseline | Mixed sparse |
|---|---:|---:|---:|---:|
| Build + store | 2,277 ms | 54.0 ms | 11,167 ms | 505.7 ms |
| Open existing index | 28.6 ms | 0.985 ms | 230.6 ms | 22.3 ms |
| No-change refresh + store | 2,273 ms | 0.773 ms | 11,385 ms | 2.605 ms |
| One-file refresh + store | 2,264 ms | 11.9 ms | 11,409 ms | 13.2 ms |
| Common `dispatch` query | 4.786 ms | 0.900 ms | 23.754 ms | 3.855 ms |
| Common `Result<Response>` query | 7.469 ms | 0.900 ms | 35.448 ms | 3.904 ms |
| Unique identifier query | 2.724 ms | 0.000875 ms | 12.466 ms | 0.000792 ms |
| Missing identifier query | 2.416 ms | 0.000375 ms | 11.452 ms | 0.000375 ms |
| Initial index bytes | 6,340,273 | 2,801,007 | 46,325,736 | 51,529,375 |

The common queries produce 16,384 and 65,536 verified matches, respectively.
Unique/missing query results are one/zero in both implementations. Submicrosecond
measurements have timer and scheduling sensitivity; the large ratios should not
be extrapolated to real user workloads.

The main gains come from document-level postings, selective intersections,
removing index-wide automaton construction, and writing only changed documents.
The biggest measured regression is **11.2% more disk space on mixed input**.
Sparse keys and uncompressed archives do not guarantee a smaller index.

### Accumulated updates and compaction

A separate synthetic sequence updates one of 64 small files 63 times. At 64
segments, opening took 3.423 ms, a warm query 20.125 µs, and storage 224,985 bytes.
Compaction took 17.202 ms, leaving one 82,398-byte segment; opening then took
0.244 ms and the same 64-result query took 10.750 µs. See
[segment observations](benchmarks/segments-m3-2026-09-17.json). Intermediate
query samples were noisy, so this does not establish an optimal segment limit.

Using those recorded endpoints, compaction pays for itself after approximately
**six subsequent opens**, or **1,835 warm queries** with no further edits.
This is an offline cost estimate, not measured future behavior. It explains why
an eventual automatic policy should consider query/open frequency and stale
bytes, rather than compact on every edit. The current command is explicit.

### Next performance experiments, in priority order

1. **Segment policy and incremental event ingestion.** Measure mixed read/write
   traces, then choose compaction thresholds under bounded query latency and
   disk growth. A filesystem watcher can avoid O(files) metadata scans but needs
   overflow recovery and periodic reconciliation. Neither is implemented here.
2. **Startup and memory.** Reuse views in a long-lived process. Evaluate validated
   immutable mappings or selective loading before changing the owned-buffer
   safety model. Current open reads/checks all segment bytes; peak RSS and cold
   disk latency have not been measured.
3. **Index size.** Compare blocked/delta-compressed postings and document blocks
   against current zero-copy access. Require unchanged oracle results and measure
   decode cost, especially on the mixed corpus where storage regressed.
4. **Common-query output.** Add a streaming or count-only API if profiles show
   allocation/formatting dominating. Current APIs allocate complete line/path
   strings for every match. Do not compare count-only timings with full results.
5. **Query policy.** Record posting cardinalities and verification bytes, then
   replay alternative intersection orders/crossovers with ordinary code. Current
   rarest-first ordering and the 8:1 binary-search crossover remain heuristics.

## Axis 2: search quality

For an exact code-search engine, quality starts with complete, correct match
locations and fresh content. Sparse n-grams improve candidate filtering, not
relevance ranking. The correctness gate compares full `(path, line, start, end)`
locations against direct line scans, not just result counts.

The 35-query fixture deliberately targets known edge cases: short literals,
Unicode, overlapping literals, repeated matches, alternations with short/empty
branches, case-insensitive regexes, anchors, and regexes without fixed literals.
It is a regression suite, not a representative query distribution.

| Measure | Baseline | Sparse incremental |
|---|---:|---:|
| Queries with exact, duplicate-free results | 10 / 35 | 35 / 35 |
| Errors or panics | 15 | 0 |
| Correct match locations | 321 | 4,689 |
| Missing match locations | 4,368 | 0 |
| False match locations | 0 | 0 |
| Duplicate result entries | 288 | 0 |

[Per-query baseline results](benchmarks/quality-baseline.txt),
[per-query candidate results](benchmarks/quality-sparse.txt), and
[machine-readable totals](benchmarks/quality-summary.json) are retained.
The new version has 100% precision and recall **on this fixture**, with no
claim of universal correctness from a finite suite.

Additional tests check every character-boundary substring of a Unicode corpus,
thousands of generated ASCII queries, 20,000 randomized byte-substring covers,
and incremental edit histories against fresh rebuilds. Persistence tests cover
renames, deletions, text/binary transitions, unchanged refreshes, strict content
verification, corrupted archives, stale writers, concurrent readers during
compaction, and old snapshots. CLI tests exercise default refresh and explicit
snapshot behavior.

### Next quality experiments, in priority order

1. **Coverage and freshness:** larger adversarial differential fuzzing; filesystem
   timestamp precision, permission failures, interrupted publication and watcher
   overflow if a watcher is added. Keep recall at 100% against the declared
   line-oriented oracle. Power-loss fault injection has not been performed.
2. **Corpus selection:** define `.gitignore`, generated/vendor files, path filters,
   binary policy and non-UTF-8 path handling with user expectations. Currently
   hidden files are included except `.git`, and invalid UTF-8 paths error.
3. **Convenience semantics:** explicit case-insensitive literal and whole-word
   modes, then multiline search only with a clear result/offset contract.
4. **Relevance:** optional symbol/path/exact-name ranking needs a labeled query
   set and separate MRR/nDCG/Recall@k measurements. Keep complete exact search
   available. No semantic or fuzzy ranking improvement is claimed here.

## Reproduction

```sh
cargo build --release --locked --examples --bin zito
cargo run --release --locked --example quality_report -- /tmp/zito-quality-new
cargo run --release --locked --example bench_incremental
python3 scripts/benchmark.py --replay --output docs/benchmarks/m3-2026-09-17.json
```

To rerun the baseline, check out `32f8242` in an isolated worktree and copy
`examples/bench_index.rs` and `examples/quality_report.rs` into it. Replace
`Index::from(view)` in its benchmark with `Index::try_from(view)?`; its existing
`store(&self)` also accepts the benchmark's mutable binding. Build with
`cargo +stable build --release --locked --examples` and pass both binary paths
as `--baseline` and `--candidate` to `scripts/benchmark.py`, along with a fresh
`--output` path. Quality-report scratch directories must also be fresh.

Actual validation: stable Rust formatting, Clippy with warnings denied,
debug/release tests and release builds on the M3 Mac. No native platform,
permission-failure, power-loss, Linux/Windows, cold-cache, real-repository,
ranking, token-accounting or peak-memory claims are made from these fixtures.
