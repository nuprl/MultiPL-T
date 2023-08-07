import datasets
from assert_test import assert_block_start_prelude
import argparse


parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type=str, required=True)
parser.add_argument("--name", type=str, required=True)
args = parser.parse_args()

ds = datasets.load_dataset(args.dataset, split="train")

print(f"Loaded {len(ds)} examples")
ds = ds.filter(lambda x: len(x["tests"]) > 10)
ds = ds.filter(lambda x: x["coverage"] >= 95)
print(f"Filtered to {len(ds)} examples")

new_ds = {
    "content": [],
    "id": [],
}

for i, ex in enumerate(ds):
    code = ex["content"]
    tests_start = assert_block_start_prelude(ex["entrypoint"])
    code_with_tests = code + tests_start + "\n".join(ex["tests"])
    new_ds["content"].append(code_with_tests)
    new_ds["id"].append(ex["id"])

new_ds = datasets.Dataset.from_dict(new_ds)
new_ds.push_to_hub(args.name, private=True)
