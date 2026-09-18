#!/usr/bin/env python3
"""Record reproducible synthetic attempts; compare saved runs without agent calls.

Build each revision's examples/bench_index.rs with the same stable toolchain.
Run the variants sequentially to avoid competing for CPU and filesystem cache.
"""
import argparse
import json
import platform
import re
import statistics
import subprocess
import tempfile
from pathlib import Path


def measure(binary, files, mode):
    with tempfile.TemporaryDirectory(prefix="zito-benchmark-") as scratch:
        result = subprocess.run(
            [str(binary), scratch, str(files), mode],
            text=True, capture_output=True, check=True,
        )
    metrics = {}
    for line in result.stderr.splitlines():
        if match := re.fullmatch(r"METRIC (\w+) (\d+)", line):
            metrics[match[1]] = int(match[2])
        elif match := re.fullmatch(
            r"METRIC query (\S+) median_ns (\d+) p95_ns (\d+) matches (\d+)", line
        ):
            metrics.setdefault("queries", {})[match[1]] = {
                "median_ns": int(match[2]), "p95_ns": int(match[3]),
                "matches": int(match[4]),
            }
    assert len(metrics.get("queries", {})) == 4, result.stderr
    metrics["build_store_us"] = metrics["build_us"] + metrics["store_us"]
    return metrics


def summarize(records):
    for mode in sorted({r["corpus"] for r in records}):
        for variant in sorted({r["variant"] for r in records}):
            runs = [r["metrics"] for r in records if r["corpus"] == mode and r["variant"] == variant]
            print(mode, variant, json.dumps({
                key: statistics.median(run[key] for run in runs)
                for key in ["build_store_us", "stored_bytes", "open_us", "noop_update_store_us", "one_file_update_store_us"]
            }))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--baseline", type=Path)
    parser.add_argument("--candidate", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument("--replay", action="store_true", help="summarize saved observations without executing either binary")
    args = parser.parse_args()
    if args.replay:
        summarize(json.loads(args.output.read_text())["records"])
        return
    if not args.baseline or not args.candidate or args.repeats < 1:
        parser.error("provide both binaries and a positive repeat count")
    report = {"system": platform.platform(), "machine": platform.machine(), "records": []}
    for mode, files in [("repetitive", 256), ("mixed", 1024)]:
        for iteration in range(args.repeats):
            for variant, binary in [("baseline", args.baseline), ("sparse", args.candidate)]:
                metrics = measure(binary.resolve(), files, mode)
                report["records"].append({"variant": variant, "corpus": mode, "files": files, "iteration": iteration, "metrics": metrics})
                args.output.parent.mkdir(parents=True, exist_ok=True)
                args.output.write_text(json.dumps(report, indent=2) + "\n")
                print(f"recorded {mode} {variant} repeat {iteration + 1}", flush=True)
    summarize(report["records"])


if __name__ == "__main__":
    main()
