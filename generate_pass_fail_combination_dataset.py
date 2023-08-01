import datasets
import json
import gzip
from pathlib import Path
from argparse import ArgumentParser
from progressbar import progressbar
from utils import clean_sol_prompt

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
pa.add_argument("--lang", type=str, required=True)
pa.add_argument("--min_tests", type=int, default=3)
args = pa.parse_args()

passed = []
failed = []
tests = []
num_tests = []
ids = []
relative_ids = []


def make_path_iterator(): return Path(args.path).glob("**/*.results.json.gz")


r_i = 0
for path in progressbar(make_path_iterator(), max_value=len(list(make_path_iterator()))):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    e_id = int(path.stem.split("_")[1])
    results = data["results"]
    tests_code = data["tests"]

    _num_tests = None
    if args.lang == "lua":
        _num_tests = tests_code.count("lu.assertEquals")
    else:
        raise NotImplementedError("Only 'lua' is supported for now")

    if _num_tests < args.min_tests:
        continue

    passing = []
    failing = []
    for res in results:
        sol = clean_sol_prompt(args.lang, res["program"])
        if res["exit_code"] == 0:
            passing.append(sol)
        else:
            failing.append(sol)

    if len(passing) == 0 or len(failing) == 0:
        continue

    for p in passing:
        for f in failing:
            passed.append(p)
            failed.append(f)
            tests.append(tests_code)
            num_tests.append(_num_tests)
            ids.append(e_id)
            relative_ids.append(r_i)
            r_i += 1


new_ds = datasets.Dataset.from_dict(
    {
        "passed": passed,
        "failed": failed,
        "tests": tests,
        "num_tests": num_tests,
        "id": ids,
        "relative_id": relative_ids,
    }
)
print(len(new_ds))
new_ds.push_to_hub(args.name)
