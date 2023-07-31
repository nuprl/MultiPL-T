import datasets
import json
import gzip
from pathlib import Path
from argparse import ArgumentParser

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
pa.add_argument("--lang", type=str, required=True)
pa.add_argument("--min_tests", type=int, default=3)
args = pa.parse_args()

funcs_pass = []
funcs_fail = []
tests = []
num_tests = []
ids = []

for path in Path(args.path).glob("**/*.results.json.gz"):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    e_id = int(path.stem.split("_")[1])
    results = data["results"]
    tests_code = data["tests"]

    passed = []
    failed = []

    for res in results:
        func = res["program"]
        if res["exit_code"] == 0:
            passed.append(func)
        else:
            failed.append(func)

    _num_tests = None
    if args.lang == "lua":
        _num_tests = tests_code.count("lu.assertEquals")
    else:
        raise NotImplementedError("Only 'lua' is supported for now")

    num_tests.append(_num_tests)
    tests.append(tests_code)
    funcs_pass.append(passed)
    funcs_fail.append(failed)
    ids.append(e_id)

new_ds = datasets.Dataset.from_dict(
    {"id": ids, "num_tests": num_tests, "tests": tests,
        "pass": funcs_pass, "fail": funcs_fail})
new_ds = new_ds.filter(lambda x: x["num_tests"] >= args.min_tests)
new_ds = new_ds.filter(lambda x: len(x["pass"]) > 0 and len(x["fail"]) > 0)
print(len(new_ds))
new_ds.push_to_hub(args.name)
