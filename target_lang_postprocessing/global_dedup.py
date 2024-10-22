'''
This script takes a dataset of completions and deduplicates (in parallel) them using the 
ROGUE score. 
Cli provided in __main__.
'''
import random
from rouge_score import rouge_scorer
from dedup_solutions import strip_comments
import datasets
import argparse
from tqdm import tqdm
import math
import multiprocessing
import functools

def get_id_from_col(id, col):
    if col == "original_id":
        return id
    elif col == "path":
        return int(id.split("/")[-1].split("_")[1])
    else:
        raise ValueError(f"Unknown id column {col}")

def check_single_function(
    scorer,
    base_chunk: list[tuple[int, str, int]],
    dedup_threshold: float,
    new_solution: str,
) -> bool:
    for (_, sol, _) in base_chunk:
        scores = scorer.score(new_solution, sol)
        rouge_score = scores['rougeLsum'].fmeasure
        if rouge_score > dedup_threshold:
            return False
    return True

def dedup_chunk_mask(scorer, dedup_threshold: float, chunk: list[tuple[int, str]]):
    keep_mask = [True for _ in chunk]
    for i, (j, sol, _) in enumerate(chunk):
        keep_mask[i] = check_single_function(
            scorer, chunk[i+1:], dedup_threshold, sol)
    return keep_mask


def dedup_chunk(dedup_threshold: float, chunk: list[tuple[int, str, int]]):
    if len(chunk) == 1:
        return chunk
    scorer = rouge_scorer.RougeScorer(['rougeLsum'], use_stemmer=True)
    keep_mask = dedup_chunk_mask(scorer, dedup_threshold, chunk)
    return [chunk[i] for i in range(len(chunk)) if keep_mask[i]]

def group_solns_by_id(solns: list[tuple[int, str, int]]) -> list[list[tuple[int, str, int]]]: 
    solns_by_id = {}
    for (i, sol, id) in solns:
        if id not in solns_by_id:
            solns_by_id[id] = []
        solns_by_id[id].append((i, sol, id))
    return [solns for solns in solns_by_id.values()]


if __name__ == "__main__":
    cli = argparse.ArgumentParser()
    cli.add_argument("--input-dataset", type=str)
    cli.add_argument("--output-dataset", type=str)
    cli.add_argument("--chunk-size", type=int)
    cli.add_argument("--id-column", type=str, default="original_id")
    cli.add_argument("--lang", type=str)
    cli.add_argument("--nthreads", type=int, default=1)
    cli.add_argument("--same-dedup-threshold", type=float, default=0.6)
    cli.add_argument("--diff-dedup-threshold", type=float, default=0.7)
    cli.add_argument("--global-dedup-factor", type=float, default=1.0)
    cli.add_argument("--one-per-problem", action="store_true")
    cli.add_argument("--strip-parens", action="store_true")
    args = cli.parse_args()

    ds = datasets.load_dataset("json", data_files=args.input_dataset, split="train")
    stripped_content : list[list[tuple[int, str, int]]]= []
    for (i, (code, id) ) in enumerate(zip(ds["content"], ds[args.id_column])):
       soln = (i, strip_comments(code, args.lang, args.strip_parens), get_id_from_col(id, args.id_column))
       stripped_content.append(soln)
    grouped_stripped_content = group_solns_by_id(stripped_content)
    stripped_content = []
    if args.one_per_problem:
        for group in grouped_stripped_content:
            group.sort(key=lambda x: len(x[2]))
            stripped_content.append(group[0])
    else:
        print(f" #### deduping {len(grouped_stripped_content)} groups of same problem ####")
        with multiprocessing.Pool(args.nthreads) as pool:
            grouped_stripped_content = pool.map(functools.partial(dedup_chunk, args.same_dedup_threshold), grouped_stripped_content) 
        for group in grouped_stripped_content:
            stripped_content.extend(group)
        
        dedup_group_size = min(len(stripped_content) // args.nthreads, args.chunk_size) 
        dedup_rounds = int(max(math.log(dedup_group_size, 2), 5)
                        * args.global_dedup_factor)
        for i in tqdm(range(dedup_rounds)):
            print(
                f" #### global dedup round {i+1}/{dedup_rounds}. current num solutions: {len(stripped_content)} ####")
            random.shuffle(stripped_content)
            chunks = [] 
            for j in range(0, len(stripped_content), dedup_group_size):
                chunks.append(stripped_content[j:j+dedup_group_size]) 
            print(f"    # deduping {len(chunks)} groups with {dedup_group_size} solutions each #")
            with multiprocessing.Pool(args.nthreads) as pool:
                chunks = pool.map(functools.partial(dedup_chunk, args.diff_dedup_threshold), chunks)
            stripped_content = []
            for chunk in chunks:
                stripped_content.extend(chunk)
            dedup_group_size = min(len(stripped_content) // args.nthreads, args.chunk_size)
    dedup_indices = [i for (i, _, _) in stripped_content]
    dedup_ds = ds.select(dedup_indices)
    dedup_ds.to_json(args.output_dataset)
