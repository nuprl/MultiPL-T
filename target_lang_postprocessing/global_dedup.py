'''
This script takes a dataset of completions and deduplicates (in parallel) them using the 
ROGUE score. 
Cli provided in __main__.
'''
import numpy as np
from rouge_score import rouge_scorer
from dedup_solutions import strip_comments
import datasets
import argparse
from tqdm import tqdm
import multiprocessing
import functools


def check_single_function(
    scorer,
    base_chunk: list[str],
    dedup_threshold: float,
    new_solution: str,
) -> bool:
    for sol in base_chunk:
        scores = scorer.score(new_solution, sol)
        rouge_score = scores['rougeLsum'].fmeasure
        if rouge_score > dedup_threshold:
            return False
    return True


def dedup_chunk_mask(scorer, dedup_threshold: float, chunk: list[str]):
    keep_mask = [True for _ in chunk]
    for i, sol in enumerate(chunk):
        ind = min(i+1, len(chunk)-1)
        keep_mask[i] = check_single_function(
            scorer, chunk[ind:], dedup_threshold, sol)
    return keep_mask


def dedup_chunk(scorer, dedup_threshold: float, chunk: list[str]):
    keep_mask = dedup_chunk_mask(scorer, dedup_threshold, chunk)
    return [chunk[i] for i in range(len(chunk)) if keep_mask[i]]


def compare_chunk(scorer, dedup_threshold, comp_chunk, base_chunk):
    return [check_single_function(scorer, base_chunk, dedup_threshold, fn) for fn in comp_chunk]


if __name__ == "__main__":
    cli = argparse.ArgumentParser()
    cli.add_argument("--input-dataset", type=str)
    cli.add_argument("--output-dataset", type=str)
    cli.add_argument("--chunk-size", type=int)
    cli.add_argument("--lang", type=str)
    cli.add_argument("--nthreads", type=int, default=1)
    cli.add_argument("--dedup-threshold", type=float, default=0.6)
    cli.add_argument("--strip-parens", action="store_true")
    args = cli.parse_args()

    ds = datasets.load_dataset(
        "json", data_files=args.input_dataset, split="train")
    stripped_content = [strip_comments(
        code, args.lang, args.strip_parens) for code in ds["content"]]
    chunks = [stripped_content[i:min(i+args.chunk_size, len(stripped_content))]
              for i in range(0, len(stripped_content), args.chunk_size)]
    scorer = rouge_scorer.RougeScorer(['rougeLsum'], use_stemmer=True)
    with multiprocessing.Pool(args.nthreads) as pool:
        dedup_chunks = []
        print(f"Deduping withinin chunks: {len(chunks)}")
        for i in tqdm(range(0, len(chunks), args.nthreads)):
            chunk_slice = chunks[i:min(i+args.nthreads, len(chunks))]
            dedup = pool.map(
                functools.partial(dedup_chunk, scorer, args.dedup_threshold),
                chunks
            )
            for dchunk in dedup:
                dedup_chunks.append(dchunk)
        keep_mask = []
        print(f"Deduped within chunks. Now deduping across chunks")
        for i, chunk in tqdm(enumerate(dedup_chunks), total=len(dedup_chunks)):
            all_masks = []
            for j in range(i+1, len(dedup_chunks), args.nthreads):
                masks = pool.map(
                    functools.partial(compare_chunk, scorer, args.dedup_threshold, chunk),
                    dedup_chunks[j:min(i+1+args.nthreads, len(dedup_chunks))]
                )
                for m in masks:
                    all_masks.append(m)
            for j in len(chunk):
                isset = False
                for mask in all_masks: 
                    if not m[j]:
                        keep_mask.append(False)
                        break
                if not isset: 
                    keep_mask.append(True)
                
    dedup_ds = ds.select([i for i, b in enumerate(keep_mask) if b])
    dedup_ds.to_json(args.output_dataset)
