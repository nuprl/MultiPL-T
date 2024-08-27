import datasets
from pathlib import Path
import argparse


def extract_prefix(split, content):
    if split == "racket":
        lines = content.split("\n")
        prefix = []
        for line in lines:
            prefix.append(line)
            if line.startswith("(define"):
                break
        return "\n".join(prefix)
    else:
        raise ValueError(f"Unsupported split: {split}")


def adapt_to_chat(source_dataset: datasets.DatasetDict, split: str) -> datasets.Dataset:
    """
    For each item in source_dataset, item["content"] is a string.

    Turn each string into this format:

    {
        "messages": [
            {
                "role": "system",
                "content": "You are an expert programmer."
            },
            {
                "role": "user",
                "content": "Complete the following function:\n\n``\nPREFIX`\n```"
            },
            {
                "role": "assistant",
                "content": "Here is the code:\n\n```\nCONTENT\n```"
            }
        ]
    }
    
    CONTENT is item["content"].


    THE PREFIX consists of the lines up to and including the first line
    that begins with `(define`..
    """
    def adapt_item(item):
        content = item["content"]
        prefix = extract_prefix(split, content)

        return {
            "messages": [
                {
                    "role": "system",
                    "content": "You are an expert programmer."
                },
                {
                    "role": "user",
                    "content": f"Complete the following function:\n\n```\n{prefix}\n```"
                },
                {
                    "role": "assistant",
                    "content": f"Here is the code:\n\n```\n{content}\n```"
                }
            ]
        }

    return source_dataset[split].map(adapt_item).remove_columns(source_dataset[split].column_names)

def main_with_args(dataset_name: str, split: str, output_path: str):
    dataset = datasets.load_dataset(dataset_name)
    adapted_dataset = adapt_to_chat(dataset, split)
    adapted_dataset.to_json(output_path)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset-name", type=str, default="nuprl/MultiPL-T")
    parser.add_argument("--split", type=str, required=True)
    parser.add_argument("--output-path", type=str, required=True)
    args = parser.parse_args()
    main_with_args(args.dataset_name, args.split, args.output_path)

if __name__ == "__main__":
    main()
    