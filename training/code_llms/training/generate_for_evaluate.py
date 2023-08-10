from typing import List
import torch
from pathlib import Path
import json
import gzip
from torch.utils.data.dataloader import DataLoader
from tqdm import tqdm

def _stop_at_stop_token(decoded_string: str, stop_tokens: List[str]) -> str:
    """
    Produces the prefix of decoded_string that ends at the first occurrence of
    a stop_token.

    WARNING: the decoded_string *must not* include the prompt, which may have stop tokens
    itself.
    """
    min_stop_index = len(decoded_string)
    for stop_token in stop_tokens:
        stop_index = decoded_string.find(stop_token)
        if stop_index != -1 and stop_index < min_stop_index:
            min_stop_index = stop_index
    return decoded_string[:min_stop_index]


class GenerateForEvaluate:
    """
    This class can be used as the evaluation function for the trainer in trainer.py.

    """

    def __init__(self, model, tokenizer, test_dataloader, num_return_sequences, stop_tokens):
        self.test_dataloader = test_dataloader
        self.tokenizer = tokenizer
        self.model = model
        self.num_return_sequences = num_return_sequences
        self.stop_tokens = stop_tokens
        self.max_length = 512

    
    @classmethod
    def from_jsonl(cls, model, tokenizer, jsonl_file: Path, batch_size, num_return_sequences):
        test_data = [ ]
        stop_tokens = None
        stop_tokens_tuple = None
        with jsonl_file.open() as f:
            for line in f:
                item = json.loads(line)
                if stop_tokens is None:
                    stop_tokens = item["stop_tokens"]
                    stop_tokens_tuple = tuple(stop_tokens)
                else:
                    assert stop_tokens_tuple == tuple(item["stop_tokens"])
                del item["stop_tokens"]
                test_data.append(item)
        data_loader = DataLoader(test_data, batch_size)
        return cls(model, tokenizer, data_loader, num_return_sequences, stop_tokens)

    def __call__(self, checkpoint_dir: Path, step):
        if checkpoint_dir is None:
            return
        self.model.eval()
        eval_dir = (checkpoint_dir / f"eval_{step}")
        eval_dir.mkdir(exist_ok=True, parents=True)
        for batch in tqdm(self.test_dataloader):
            for i in range(len(batch["prompt"])):
                # Reconstruct the item
                item = {}
                for key in batch:
                    item[key] = batch[key][i]
                prompt = item["prompt"]
                inputs = self.tokenizer(
                    prompt, return_tensors="pt", return_token_type_ids=False
                ).to(self.model.device)
                output_tokens = self.model.generate(
                    **inputs,
                    num_return_sequences=self.num_return_sequences,
                    max_length=self.max_length,
                    do_sample=True,
                    temperature=0.2,
                    use_cache=True,
                    pad_token_id=self.tokenizer.eos_token_id
                )
                outputs = self.tokenizer.batch_decode(
                    output_tokens,
                    clean_up_tokenization_spaces=False,
                    skip_special_tokens=True,
                )
                item["stop_tokens"] = self.stop_tokens
                item["completions"] = [
                    _stop_at_stop_token(output[len(prompt):], item["stop_tokens"])
                    for output in outputs
                ]

                completions_path = eval_dir / f"{item['name']}.json.gz"
                
                with gzip.open(completions_path, mode="wt") as f:
                    f.write(json.dumps(item, indent=2))
        self.model.train()
