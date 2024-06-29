"""
Use this script to estimate the number of items in a dataset after packing. For
example:

python3 -m llm_basics.estimate_packed_steps \
    --tokenizer bigcode/starcoder2-15b \
    --dataset nuprl/MultiPL-T \
    --split lua

"""
from .train.packed_strings_dataset import PackedStringsDataset
from transformers import AutoTokenizer
from argparse import ArgumentParser
from datasets import load_dataset

def main_with_args(tokenizer, dataset, split, max_seq_length):
    dataset = load_dataset(dataset, split=split)
    tokenizer = AutoTokenizer.from_pretrained(tokenizer)
    tokenizer.pad_token = tokenizer.eos_token
    dataset.set_transform(
        lambda item: tokenizer(item["content"], return_attention_mask=False)
    )

    tokenized_dataset = PackedStringsDataset(
        dataset,
        max_seq_length,
        tokenizer.eos_token_id,
        epochs=1,
        skip_last_batch=False,
        device="cpu",
    )

    max_steps = tokenized_dataset.set_length_with_approximate_count()
    print("Estimated packed items:", max_steps, "with", max_seq_length, " tokens per item.")

def main():
    parser = ArgumentParser()
    parser.add_argument("--tokenizer", type=str, required=True)
    parser.add_argument("--dataset", type=str, required=True)
    parser.add_argument("--split", type=str, default="train")
    parser.add_argument("--max_seq_length", type=int, default=2048)
    main_with_args(**vars(parser.parse_args()))

if __name__ == "__main__":
    main()