import gzip
from tqdm import tqdm
import os
import json
from pathlib import Path


prompt_file = "./r-prompts.jsonl"
with open(prompt_file, "r") as f:
    prompts = [json.loads(line) for line in f]

name_to_prompt = {p["name"]: p["prompt"] for p in prompts}

missed = 0
for path in tqdm(Path("./r_data").glob("**/*.json.gz")):
    if ".results" in path.stem:
        os.remove(path)
        continue
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    if data["name"] not in name_to_prompt:
        missed += 1
        os.remove(path)
        continue
    new_prompt = name_to_prompt[data["name"]]
    data["prompt"] = new_prompt

    with gzip.open(path, "wt") as f:
        json.dump(data, f)

print(f"Missed {missed} prompts")
