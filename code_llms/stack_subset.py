import datasets
import argparse
from pathlib import Path
import json
from typing import Optional
from tqdm import tqdm

def fetch_subset(data_dir: str, items: Optional[int], size: Optional[int], output: Path):
    data = datasets.load_dataset("bigcode/starcoderdata", data_dir=data_dir, streaming=True)
    data = data["train"]
    total_size = 0
    with output.open("w") as f:
        for i, item in tqdm(enumerate(data)):
            json.dump(item, f)
            f.write("\n")
            total_size += len(item["content"])
            if items is not None and i == items:
                break
            if size is not None and total_size * 1024 >= size:
                break


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--data-dir", type=str, required=True)
    parser.add_argument("--items", type=int)
    parser.add_argument("--size", type=int)
    parser.add_argument("--output", type=Path, required=True)

    args = parser.parse_args()
    args = vars(args)
    fetch_subset(**args)

if __name__ == "__main__":
    main()
        
