import datasets
import json
from pathlib import Path
import argparse

def get_id(path):
    return path.split("/")[-1].split("_")[1]

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset", type=str, required=True, help="Dataset being created")
    parser.add_argument("--json-dir", type=str, required=True, help="Directory containing jsonl files")
    parser.add_argument("--amend", action="store_true", help="Ammend existing dataset, if not present, create new dataset")
    parser.add_argument("--push", action="store_true", help="Push to HuggingFace Datasets")
    parser.add_argument("--local-file", type=str, help="Local file to save in addition/instead-of to pushing to HuggingFace Datasets")
    args = parser.parse_args()
    if not args.push and args.local_file is None:
        print("Must specify --push or --local-file")
        exit(1)
    if args.amend:
        try: 
            ds = datasets.load_dataset(args.dataset, split = "train")
            seen = set(ds["id"])
        except Exception as e: 
            print(f"Could not pull dataset {args.dataset} from HuggingFace Datasets")
            print(e)
            exit(1)
    else: 
        ds = datasets.Dataset.from_dict({})
        seen = set()

    new_ds_dict = {
        "content": [], 
        "id": [],
        "path": [],
        "attempts": [],
    }
    for file in Path(args.json_dir).glob("*.jsonl"):
        print(f"Processing {file}")
        with open(file, "r") as f:
            data = [json.loads(line) for line in f]
        for d in data: 
            id = get_id(d["path"])
            if not id in seen:
                seen.add(id)
                new_ds_dict["content"].append(d["content"])
                new_ds_dict["path"].append(d["path"])
                new_ds_dict["id"].append(id)
                try:
                    new_ds_dict["attempts"].append(d["attempts"])
                except KeyError:
                    new_ds_dict["attempts"].append(None)
    new_ds = datasets.Dataset.from_dict(new_ds_dict)       
    final_ds = datasets.concatenate_datasets([ds, new_ds])

    if args.push:
        final_ds.push_to_hub(args.dataset, private=True)
    elif args.local_file.endswith(".jsonl"):
        final_ds.to_json(args.local_file)
    else:
        final_ds.save_to_disk(args.local_file)
