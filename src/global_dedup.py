'''
This script takes a dataset of completions and deduplicates (in parallel) them using the 
ROGUE score. 
Cli provided in __main__.
'''
import numpy as np
from rouge_score import rouge_scorer
import datasets 
import argparse
from tqdm import tqdm
import multiprocess


def strip_comments(code: str, lang: str):
    comment_prefix = {
        "lua": "--",
        "python": "#",
        "javascript": "//",
        "racket": ";",
        "ml": "(*",
    }
    comment_postfix = {
        "lua": "",
        "python": "",
        "javascript": "",
        "racket": "",
        "ml": "*)",
    }

    # Get comment prefix and postfix for given language
    prefix = comment_prefix.get(lang)
    postfix = comment_postfix.get(lang)

    if not prefix:
        raise ValueError(f"Language {lang} not supported")

    # If comment postfix is not empty, handle multi-line comments (like OCaml)
    if postfix:
        comment_start = prefix
        comment_end = postfix
        while comment_start in code and comment_end in code:
            start = code.find(comment_start)
            end = code.find(comment_end, start + len(comment_start))
            if start != -1 and end != -1:  # Ensure both comment start and end are found
                code = code[:start] + code[end + len(comment_end):]
            else:
                break
    else:
        # If comment postfix is empty, handle single-line comments
        lines = code.split("\n")
        lines = [line for line in lines if not line.lstrip().startswith(prefix)]
        code = "\n".join(lines)

    return code

def check_single_function(
        base_chunk: list[str], 
        new_solution, 
        dedup_threshold=0.6,
        ingnore_index=-1
    ) -> bool:
    scorer = rouge_scorer.RougeScorer(['rougeLsum'], use_stemmer=True)
    exists_ignore_index = ingnore_index >= 0
    for i, sol in enumerate(base_chunk):
        if exists_ignore_index and i == ingnore_index:
            continue
        scores = scorer.score(new_solution, sol)
        rouge_score = scores['rougeLsum'].fmeasure
        if rouge_score > dedup_threshold:
            return False
    return True

def dedup_chunk_mask(chunk, dedup_threshold):
    keep_mask = [True for _ in chunk]
    for i, sol in enumerate(chunk):
        ind = max(i+1, len(chunk)-1)
        keep_mask[i] = check_single_function(chunk[ind:], sol, dedup_threshold, i)
    return keep_mask

def dedup_chunk(chunk, dedup_threshold):
    keep_mask = dedup_chunk_mask(chunk, dedup_threshold)
    return [chunk[i] for i in range(len(chunk)) if keep_mask[i]]






if __name__ == "__main__":
    cli = argparse.ArgumentParser()
    cli.add_argument("--input-dataset", type=str)
    cli.add_argument("--output-dataset", type=str)
    cli.add_argument("--dedup-threshold", type=float, default=0.6)
    cli.add_argument("--nthreads", type=int, default=1)
    cli.add_argument("--lang", type=str)
    args = cli.parse_args()

    ds = datasets.load_dataset("json", data_files=args.input_dataset, split="train")
    stripped_content = [ strip_comments(code, args.lang) for code in ds["content"] ]
    chunk_size = len(stripped_content) // args.nthreads
    chunks = [stripped_content[i:i+max(chunk_size, len(stripped_content))] for i in range(0, len(stripped_content), chunk_size)]
    with multiprocess.Pool(args.nthreads) as pool:
        dedup_chunks = pool.map(
            lambda chunk: dedup_chunk(chunk, args.dedup_threshold),
            chunks
        )
        keep_mask = [True for _ in stripped_content]
        print(f"Deduped within chunks. Chunk size:{chunk_size}. Now deduping across chunks")
        for i, chunk in tqdm(enumerate(dedup_chunks)):
            for bchunk in dedup_chunks[i+1:]:
                is_dup = pool.map(
                    lambda soln: check_single_function(bchunk, soln, args.dedup_threshold), 
                    chunk
                )
                for j in range(i, i+len(chunk)):
                    if is_dup[j-i]:
                        keep_mask[j] = False
    dedup_ds = ds.select(keep_mask)
    dedup_ds.to_json(args.output_dataset)
                 
            
    
