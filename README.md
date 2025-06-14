# Zito

Does search code, fast.

## Usage

In order to find code use:
```bash
Usage: zito find [OPTIONS] <QUERY> [SEARCH_LOC]

Arguments:
  <QUERY>       The query to search for
  [SEARCH_LOC]  Which folder to search in [default: ./]

Options:
  -i, --index-dir <INDEX_DIR>  In which folder is or will the index.zito file be stored Make sure it is the index that was used to index the search_loc [default: ./]
  -r, --regex                  Whether to interpret the query as a regex
  -h, --help                   Print help
```

As an example you can search this very codebase like this:
```bash
zito find main ./
```

## Current performance

Here is the execution of the current state on a 13inch Macbook Air M3 2024 with 16GB RAM.

Searching the linux kernel source code for `file` like this (pre-existing index):
```bash
❯ cargo run --release -- find file ./linux/kernel -i ./linux/kernel/
    Finished `release` profile [optimized] target(s) in 0.03s
     Running `target/release/zito find file ./linux/kernel -i ./linux/kernel/`
Searching for query: file
Found 6076 matches in 10937 microseconds.
```
