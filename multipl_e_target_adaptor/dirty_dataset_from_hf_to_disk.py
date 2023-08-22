import argparse
import os
import datasets

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type=str,
                    default="nuprl/python-testgen-dirty-perfect")
args = parser.parse_args()

ds = datasets.load_dataset(args.dataset, split="train")
dir_of_this_script = os.path.dirname(os.path.realpath(__file__))
up = os.path.dirname(dir_of_this_script)
path = up + "/stack-dirty-python"

if not os.path.exists(path):
    os.mkdir(path)

for ex in ds:
    name = ex["name"]
    content = ex["content"]
    with open(path + "/" + name + ".py", "w") as f:
        f.write(content)
