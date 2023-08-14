import datasets
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type=str, required=True)
parser.add_argument("--entrypoint_col", type=str, default="entrypoint")
parser.add_argument("--name", type=str, required=True)
args = parser.parse_args()

dataset = datasets.load_dataset(args.dataset, split="train")

entrypoint_set = set()


def dedup(x):
    if x[args.entrypoint_col] in entrypoint_set:
        return False
    entrypoint_set.add(x[args.entrypoint_col])
    return True


before_len = len(dataset)
dataset = dataset.filter(dedup)
after_len = len(dataset)

print(f"Before: {before_len}, After: {after_len}")
dataset.push_to_hub(args.name)
