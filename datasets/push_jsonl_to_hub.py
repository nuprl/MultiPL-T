import datasets
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--path", type=str, required=True)
parser.add_argument("--name", type=str, required=True)
args = parser.parse_args()

dataset = datasets.load_dataset("json", data_files=args.path, split="train")
dataset.push_to_hub(args.name, private=True)
