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

# TODO: do this for other langs that are not lua


def clean_sol_prompt(sol):
    assert args.lang == "lua", "Gotta implement this for other langs"
    sol = sol[:sol.find("\nlu =")]
    sol_lines = sol.split("\n")
    if "** Canonical Python Solution **" in sol_lines[0]:
        # remove the canonical solution from comment
        # canonical solution lines start with " * "
        not_canonical_i = 0
        for i, line in enumerate(sol_lines):
            if not line.startswith("-- *"):
                not_canonical_i = i
                break
        sol_lines = sol_lines[not_canonical_i:]

    # remove every line that is empty
    sol_lines = [line for line in sol_lines if line.rstrip() !=
                 "--" and line != ""]

    sol = "\n".join(sol_lines)

    print(sol)
    return sol


for path in Path(args.path).glob("**/*.results.json.gz"):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    results = data["results"]

    solns = []
    for res in results:
        if res["exit_code"] == 0:
            sol = clean_sol_prompt(res["program"])
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
#  new_ds.push_to_hub(args.name)
