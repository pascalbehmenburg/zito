# Incremental sparse index

Status: accepted for this implementation, 2026-09-17.

## Context

The prior occurrence-level trigram hash sets recorded a posting for every byte
position. Search built an Aho-Corasick automaton over the entire index, gathered
unions of candidate postings, then re-grouped them by file and line. Extend
added postings without deleting superseded ones. Storage recompressed the full
archive on every update. Unchecked archives and fabricated static references
made corruption and lifetime reasoning unnecessarily difficult.

## Decision

Adapt the monotonic-stack algorithm from `sparse_ngram` revision
`980469f26c1e18478da7a015686c43ac6f115c6e` into a private byte module. Vendor this
small algorithm with both MIT notices instead of adding the unoptimized
string-slicing crate and its unused CRC dependency. Preserve bigram ordering
and query covering rules; cap grams at 16 bytes, retain bigrams for bounded
cover fallbacks, and expire anchors that cannot produce bounded grams.

Store stable 64-bit byte fingerprints and sorted, unique 32-bit document IDs.
A collision can only admit additional candidates: exact literal/regex
verification remains mandatory. Deduplicate per document with a hash set;
sort the unique keys for deterministic construction. Choose the rarest posting
list first, intersect with linear merging for comparable lists and binary
search for highly skewed lists. The crossover is a heuristic, not a proven
optimal constant. Changing gram generation, fingerprinting or archive schema
requires a format-version change.

Persist immutable delta segments with canonical document paths, content,
metadata and line starts. Paths resolve to the latest segment; tombstones remove
older versions. This avoids mutating or rebuilding old posting lists. Publication
writes and syncs the segment, syncs the directory on Unix, then atomically
replaces and syncs the manifest. Readers acquire a shared lock during open;
writes and compaction take the exclusive lock and verify the original manifest.
Initial store refuses to overwrite; explicit replacement is a separate action.

Load into owned aligned buffers, validate archives and posting/line invariants,
and check CRC32 before exposing a view. One private `access_unchecked` follows
successful validation and borrows only immutable owned storage. This avoids
self-references and mmap hazards from external file truncation.

## Consequences

In-memory refresh and persistence are incremental. A filesystem walk still
costs O(number of files), and opening costs O(total segment bytes). Stable
readers need no lock during searches; long-lived processes can reuse a view.
No automatic compaction, watcher, memory mapping, compression, or parallel
indexing is added without workload evidence. Explicit compaction bounds stale
segments when invoked, but briefly retains both old and new data in memory.

Query semantics remain exact and line-oriented. Supporting short literals and
unfilterable regexes costs a scan instead of losing results. Candidate
selectivity is a performance measure; relevance is a separate future feature.

This changes the disk format and the previously exposed internal Rust types.
Legacy indices are rejected with explicit rebuild instructions. Metadata-based
refresh is fast but cannot prove content identity on all filesystems; strict
verification is available. Non-UTF-8 paths are rejected rather than conflated
through lossy conversion. CRC32 detects accidental corruption, not malicious
modification. Power-loss durability and non-Unix filesystem behavior require
separate platform tests.
