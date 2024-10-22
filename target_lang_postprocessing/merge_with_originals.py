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
    ids_to_file[e_id] = get_impl(file.read_text())


new_ds = []
for sample in dataset:
    e_id = sample["id"]
    if e_id not in ids_to_file:
        continue
    impl = ids_to_file[e_id]

    new_ds.append({"original": impl, **sample})


new_ds = datasets.Dataset.from_list(new_ds)
print(new_ds)

new_ds.push_to_hub(args.push, private=True)
