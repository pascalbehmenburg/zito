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
