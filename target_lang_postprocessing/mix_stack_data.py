import datasets
import argparse
from transformers import AutoTokenizer


argparser = argparse.ArgumentParser()
argparser.add_argument("--percentage_stack", type=float, default=0.25)
argparser.add_argument("--push", type=str, default="nuprl/MultiPL-T-mixed")
argparser.add_argument("--tokenizer", type=str,
                       default="bigcode/starcoderbase-1b")
args = argparser.parse_args()

multiplt = datasets.load_dataset("nuprl/MultiPL-T")
tokenizer = AutoTokenizer.from_pretrained(args.tokenizer)

name_conversion = {
    "lua": "lua",
    "ocaml": "ml",
    "racket": "scheme",
}

stack_subsets = {}
for sub in name_conversion.keys():
    ds = datasets.load_dataset(
        f"bigcode/the-stack-dedup", split="train", data_dir=f"data/{sub}")
    stack_subsets[sub] = ds

print(stack_subsets)

# compute total tokens in multiplt
token_map = {}

for sub in multiplt:
    print(f"Tokenizing {sub}")
    token_map[sub] = 0
    ds = multiplt[sub]
    for e in ds:
        token_map[sub] += len(tokenizer.tokenize(e["content"]))

print(token_map)


combined_splits = {}
for sub in multiplt:
    stack_sub = stack_subsets[sub]
    multiplt_sub = multiplt[sub]
    size_multiplt = token_map[sub]
    print(f"Size of {sub} is {size_multiplt}")

    # after combining, we need to end up with args.percentage_stack of the size
    # being the stack, and the rest being the multiplt
    get_stack = size_multiplt * args.percentage_stack
    print(f"Size we get out of stack {sub} is {get_stack} (maximum)")

    examples = {"content": []}
    current_count = 0
    for e in stack_sub:
        tokens = len(tokenizer.tokenize(e["content"]))
        if current_count + tokens > get_stack:
            break

        current_count += tokens
        examples["content"].append(e["content"])

    print(f"Size of stack {sub} is {current_count}")

    examples_ds = datasets.Dataset.from_dict(examples)
    # combine the two datasets
    combined_ds = datasets.concatenate_datasets([examples_ds, multiplt_sub])
    print(f"Number of examples in multiplt {sub} is {len(multiplt_sub)}")
    print(f"Number of examples in combined {sub} is {len(combined_ds)}")

    combined_splits[sub] = combined_ds

combined_splits = datasets.DatasetDict(combined_splits)
combined_splits.push_to_hub(args.push, private=True)
