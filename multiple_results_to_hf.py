import datasets
import json
import gzip
from pathlib import Path
from typing import Generator
from dedup_solutions import dedup
from argparse import ArgumentParser

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
pa.add_argument("--dedup", action="store_true")
pa.add_argument("--lang", type=str)
pa.add_argument("--dedup_threshold", type=float, default=0.6)
args = pa.parse_args()

if args.dedup:
    assert args.lang is not None, "Must specify language for deduplication"

solutions = []

for path in Path(args.path).glob("**/*.results.json.gz"):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    results = data["results"]

    solns = []
    for res in results:
        if res["exit_code"] == 0:
            sol = res["program"]
            sol = sol[:sol.find("\nlu =")]
            if "TODO" in sol:
                continue
            solns.append(sol)

    if args.dedup:
        solns = dedup(solns, args.lang, args.dedup_threshold)

    print(f"{path}: {len(solns)} solutions")
    solutions.extend(solns)


new_ds = datasets.Dataset.from_dict(
    {"content": solutions, "id": list(range(len(solutions)))})
print(len(new_ds))
new_ds.push_to_hub(args.name)
