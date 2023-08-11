from pathlib import Path
import datasets
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type=str, required=True)
parser.add_argument("--percent", type=float, required=True)
parser.add_argument("--name", type=str, required=True)
parser.add_argument("--seed", type=int, default=42)
args = parser.parse_args()

raw_data = datasets.load_dataset(args.dataset, split="train")
raw_data = raw_data.shuffle(
    seed=args.seed)
sds = raw_data.train_test_split(test_size=args.percent, seed=args.seed)
sds["train"] = sds["train"].map(
    lambda x: {"content": "\n".join(x["content"].split("\n")[1:])}
)
train_data = datasets.concatenate_datasets(
    [sds["train"], sds["test"]]).shuffle()
train_data.push_to_hub(
    args.name,
    private=True,
)
