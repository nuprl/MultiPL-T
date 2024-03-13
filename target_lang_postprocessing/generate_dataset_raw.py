import datasets
import re
import json
import gzip
from pathlib import Path
from argparse import ArgumentParser
from tqdm import tqdm
from utils import clean_sol_prompt

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
pa.add_argument("--lang", type=str, required=True)
args = pa.parse_args()

funcs = []
ids = []


def make_path_iterator(): return Path(args.path).glob("**/*.results.json.gz")


RE_DIGITS = re.compile(r"\d+")
for path in tqdm(make_path_iterator(), total=len(list(make_path_iterator()))):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    # get first number in path.stem
    re_result = RE_DIGITS.search(path.stem)
    assert re_result is not None, f"Could not find number in {path.stem}"
    e_id = int(re_result.group(0))
    results = data["results"]
    tests_code = data["tests"]

    sols = []

    for res in results:
        sol = clean_sol_prompt(args.lang, res["program"])
        sols.append(sol)

    funcs.append(sols)
    ids.append(e_id)

new_ds = datasets.Dataset.from_dict(
    {
        "id": ids,
        "content": funcs,
    }
)
print(len(new_ds))
new_ds.push_to_hub(args.name, private=True)
