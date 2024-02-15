import datasets
from pathlib import Path
import argparse


def get_impl(code: str):
    lines = code.split("\n")
    save = []
    for line in lines:
        if "### Canonical solution below ###" in line:
            continue
        if "### Unit tests below ###" in line:
            break
        save.append(line)

    return "\n".join(save).strip()


parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type=str, required=True)
parser.add_argument("--push", type=str, required=True)
parser.add_argument("--originals_dir", type=str, required=True)
args = parser.parse_args()

dataset = datasets.load_dataset(args.dataset, split="train")


# Load the original files


ids_to_file = {}
for file in Path(args.originals_dir).rglob("*"):
    e_id = int(file.name.split("_")[1])
    print(e_id)
