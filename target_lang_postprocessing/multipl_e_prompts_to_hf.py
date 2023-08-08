import datasets
import json
from pathlib import Path
from dedup_solutions import rouge_dedup
from argparse import ArgumentParser

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
args = pa.parse_args()

new_ds = {
    "name": [],
    "language": [],
    "prompt": [],
    "doctests": [],
    "original": [],
    "prompt_terminology": [],
    "tests": [],
    "stop_tokens": []
}

with open(args.path, "r") as f:
    ds_lines = f.readlines()

for line in ds_lines:
    line = json.loads(line)
    new_ds["name"].append(args.name)
    new_ds["language"].append(line["language"])
    new_ds["prompt"].append(line["prompt"])
    new_ds["doctests"].append(line["doctests"])
    new_ds["original"].append(line["original"])
    new_ds["prompt_terminology"].append(line["prompt_terminology"])
    new_ds["tests"].append(line["tests"])
    new_ds["stop_tokens"].append(line["stop_tokens"])

new_ds = datasets.Dataset.from_dict(new_ds)
print(new_ds.to_pandas().head())
