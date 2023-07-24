import datasets
import json
import gzip
from pathlib import Path
from typing import Generator
from dedup_solutions import dedup
from utils import clean_sol_prompt
from argparse import ArgumentParser

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
pa.add_argument("--dedup", action="store_true")
pa.add_argument("--lang", type=str, required=True)
pa.add_argument("--dedup_threshold", type=float, default=0.6)
args = pa.parse_args()

solutions = []
original_ids = []
pass_rates = []
tests = []

for path in Path(args.path).glob("**/*.results.json.gz"):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    original_id = int(path.stem.split("_")[1])
    results = data["results"]
    func_tests = data["tests"]

    solns = []
    num_failed = 0
    num_passed = 0
    for res in results:
        if res["exit_code"] == 0:
            num_passed += 1
            sol = clean_sol_prompt(args.lang, res["program"])
            if "TODO" in sol:
                continue
            solns.append(sol)
        else:
            num_failed += 1

    pass_rate = num_passed / (num_passed + num_failed)

    if args.dedup:
        solns = dedup(solns, args.lang, args.dedup_threshold)

    print(f"{path}: {len(solns)} solutions")
    solutions.extend(solns)
    pass_rates.extend([pass_rate] * len(solns))
    original_ids.extend([original_id] * len(solns))
    tests.extend([func_tests] * len(solns))


new_ds = datasets.Dataset.from_dict(
    {"content": solutions, "pass_rate": pass_rates, "id": list(range(len(solutions))), "original_id": original_ids, "tests": tests})
print(len(new_ds))
new_ds.push_to_hub(args.name)
