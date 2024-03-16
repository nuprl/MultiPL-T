from vllm import LLM, SamplingParams
from typing import List
import torch
from pathlib import Path
import json
import random

SEPARATOR = "====="

def read_jsonl(p: Path) -> List[dict]:
    with p.open("r") as f:
        return [json.loads(l) for l in f]

def select_random(dataset: List[dict], n: int) -> str:
    """
    Select n unique items from dataset.
    """
    return [ item["content"] for item in random.sample(dataset, n) ]

def build_prompt(seed_dataset: List[dict], generated_dataset: List[dict]) -> str:
    """
    Prompt has 4 items. 2 seeds and 2 from generated chosen randomly.
    If there are fewer than 10 generated items, we will use only seeds.
    """
    prompt_items = [ ]
    if len(generated_dataset) < 10:
        prompt_items.extend(select_random(seed_dataset, 4))
    else:
        prompt_items.extend(select_random(seed_dataset, 2))
        prompt_items.extend(select_random(generated_dataset, 2))

    return f"\n\n{SEPARATOR}\n\n".join(prompt_items) + f"\n\n{SEPARATOR}\n\n;;"

def main_with_args(
    model_path: str,
    seed_dataset_path: Path,
    generated_dataset_path: Path,
    max_generated: int):

    seed_dataset = read_jsonl(seed_dataset_path)
    # To support resumption
    generated_dataset = read_jsonl(generated_dataset_path)

    model = None
    params = SamplingParams(temperature=0.8, top_p=0.95, max_tokens=512, stop=[SEPARATOR])
    
    with open(generated_dataset_path, "a") as f:
        while len(generated_dataset) < max_generated:
            prompt = build_prompt(seed_dataset, generated_dataset)
            if model is None:
                model = LLM(model=model_path, dtype=torch.bfloat16, gpu_memory_utilization=0.95)
            outputs = model.generate(prompt, params, use_tqdm=False)
            output = ";;" + outputs[0].outputs[0].text.rstrip()
            new_item = { "content": output }
            generated_dataset.append(new_item)
            f.write(json.dumps(new_item) + "\n")
            f.flush()
            print(f"Generated 1 new item. Total: {len(generated_dataset)}")
    
def main():
    main_with_args(
        model_path="/work/arjunguha-research-group/arjun/models/starcoderbase",
        seed_dataset_path=Path("./multipl_humaneval_rkt_seeds.jsonl"),
        generated_dataset_path=Path("./generated.jsonl"),
        max_generated=50_000
    )

if __name__ == "__main__":
    main()
