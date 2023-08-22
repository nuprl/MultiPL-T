import datasets
import argparse
import pathlib
import os

parser = argparse.ArgumentParser()
parser.add_argument("--push", type=str, required=True)
args = parser.parse_args()

dir_of_this_script = os.path.dirname(os.path.realpath(__file__))
files = pathlib.Path(dir_of_this_script + "/stack-clean-python").glob("*.py")

ds = {
    "content": [],
    "name": [],
}

for file in files:
    with open(file, "r") as f:
        ds["content"].append(f.read())
        ds["name"].append(file.stem)


ds = datasets.Dataset.from_dict(ds)
ds.push_to_hub(args.push, private=True)
