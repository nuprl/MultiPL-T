# this script was used to add the coverage column to the dataset, as it was not included in the original dataset
import datasets

ds_name = "nuprl/leetcode-solutions-python-testgen"
dataset = datasets.load_dataset(ds_name, split="train")


#  new_ds = {
#  "content": [],
#  "sha1": [],
#  "id": [],
#  "entrypoint": [],
#  "tests": [],
#  "coverage": [],
#  "tests_failed": []
#  }

#  for i, ex in enumerate(dataset):
#  new_ds["content"].append(ex["content"])
#  new_ds["sha1"].append(ex["sha1"])
#  new_ds["id"].append(ex["id"])
#  new_ds["entrypoint"].append(ex["entrypoint"])
#  new_ds["tests"].append(ex["tests"])
#  new_ds["coverage"].append(ex["coverage"])
#  new_ds["tests_failed"].append(None)

new_ds = {
    "content": [],
    "sha1": [],
    "id": [],
    "entrypoint": [],
    "tests": [],
    "coverage": [],
    "difficulty": [],
    "problem_name_coded": [],
    "problem_name_worded": [],
    "full_code": [],
    "signature": [],
    "prompt": [],
    "imports": [],
    "was_merged": [],
    "tests_failed": [],
}

for i, ex in enumerate(dataset):
    new_ds["content"].append(ex["content"])
    new_ds["sha1"].append(ex["sha1"])
    new_ds["id"].append(ex["id"])
    new_ds["entrypoint"].append(ex["entrypoint"])
    new_ds["tests"].append(ex["tests"])
    new_ds["coverage"].append(ex["coverage"])
    new_ds["difficulty"].append(ex["difficulty"])
    new_ds["problem_name_coded"].append(ex["problem_name_coded"])
    new_ds["problem_name_worded"].append(ex["problem_name_worded"])
    new_ds["full_code"].append(ex["full_code"])
    new_ds["signature"].append(ex["signature"])
    new_ds["prompt"].append(ex["prompt"])
    new_ds["imports"].append(ex["imports"])
    new_ds["was_merged"].append(ex["was_merged"])
    new_ds["tests_failed"].append(None)

new_ds = datasets.Dataset.from_dict(new_ds)
new_ds.push_to_hub(ds_name)
