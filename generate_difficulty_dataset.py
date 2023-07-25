import datasets
import json
import gzip
from pathlib import Path
from argparse import ArgumentParser

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--original_dataset", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
pa.add_argument("--min_tests", type=int, default=3)
args = pa.parse_args()

ds = datasets.load_dataset(args.original_dataset, split="train")

print("WARNING: this script only works with a Lua target dataset.")

id_to_func = {}
for ex in ds:
    id_to_func[ex["id"]] = ex["content"]

funcs = []
tests = []
target_soln = []
ids = []
pass_rates = []

for path in Path(args.path).glob("**/*.results.json.gz"):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    e_id = int(path.stem.split("_")[1])
    results = data["results"]
    tests_code = data["tests"]

    num_failed = 0
    num_passed = 0
    sample = None
    for res in results:
        if res["exit_code"] == 0:
            num_passed += 1
            if sample is None:
                sample = res["program"]
        else:
            num_failed += 1

    pass_rate = num_passed / (num_passed + num_failed)
    num_tests = tests_code.count("lu.assertEquals")
    func = id_to_func[e_id]
    funcs.append(func)
    tests.append(num_tests)
    ids.append(e_id)
    pass_rates.append(pass_rate)
    target_soln.append(sample)

new_ds = datasets.Dataset.from_dict(
    {"content": funcs, "tests": tests, "pass_rate": pass_rates, "id": ids, "target_soln": target_soln})
new_ds = new_ds.filter(lambda x: x["pass_rate"] > 0.1)
new_ds = new_ds.filter(lambda x: x["tests"] >= args.min_tests)
print(len(new_ds))
new_ds.push_to_hub(args.name)
