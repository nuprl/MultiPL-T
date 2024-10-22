from typing import List, Tuple
import re
import math
import datasets
import json
import os
import gzip
import multiprocessing
import random
from tqdm import tqdm
from pathlib import Path
from dedup_solutions import rouge_dedup
from utils import clean_sol_prompt
from argparse import ArgumentParser

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
pa.add_argument("--strategy", type=str, default="dedup",
                choices=["dedup", "dedup_best", "best", "random", "all"])
pa.add_argument("--global_dedup", action="store_true")
pa.add_argument("--global_dedup_prob", type=float, default=0.35,
                help="the probability that a pair of examples will not be deduplicated, despite being similar. higher results in a more aggressive and slower deduplication.")
pa.add_argument("--lang", type=str, required=True)
pa.add_argument("--dedup_threshold", type=float, default=0.6)
pa.add_argument("--score_batch", type=int, default=32)
pa.add_argument("--processing_batch", type=int, default=512)
pa.add_argument("--score_device", type=str, default="cpu")
pa.add_argument("--no_score", action="store_true")
pa.add_argument("--no_threading", action="store_true")
args = pa.parse_args()

num_has_at_least_one_passing = 0

class Solution:
    def __init__(self, code, score, original_id, pass_rate, tests, failing=None):
        self.code = code
        self.score = score
        self.original_id = original_id
        self.pass_rate = pass_rate
        self.tests = tests
        self.failing = failing


solutions: List[Solution] = []

scorer = None
if not args.no_score:
    from code_scorer.inference import CodeScorer
    scorer = CodeScorer("nuprl/code-scorer-edu-v1", device=args.score_device)


def get_best_sol(scores, sols):
    max_score = 0
    best_sol_idx = 0
    for i, score in enumerate(scores):
        if score > max_score:
            max_score = score
            best_sol_idx = i

    return sols[best_sol_idx], max_score


def make_path_iterator(): return Path(args.path).glob("**/*.results.json.gz")


RE_DIGITS = re.compile(r"\d+")


def process_path(path):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    re_result = RE_DIGITS.search(path.stem)
    assert re_result is not None, f"Could not find number in {path.stem}"
    original_id = int(re_result.group(0))
    results = data["results"]
    func_tests = data["tests"]

    all_sols = []
    num_failed = 0
    num_passed = 0
    failing = None
    for res in results:
        if res["exit_code"] == 0:
            num_passed += 1
            sol = clean_sol_prompt(args.lang, res["program"])
            if "TODO" in sol:
                continue
            all_sols.append(sol)
        else:
            num_failed += 1
            if failing is None:
                failing = clean_sol_prompt(args.lang, res["program"])

    pass_rate = num_passed / (num_passed + num_failed)

    # simple set dedup
    sols = list(set(all_sols))

    edu_scores = []
    if len(sols) > 0:
        if args.strategy == "dedup":
            sols = rouge_dedup(
                sols, args.lang, args.dedup_threshold, trim_top_comments=False)
            edu_scores.extend([0] * len(sols))
        elif args.strategy == "dedup_best":
            # IDEA: sort the solutions by score, then dedup, so higher scoring solutions are more likely to be kept
            assert scorer is not None
            scores = scorer.score(sols)
            score_sols = list(zip(scores, sols))
            sols_to_score = {sol: score for score, sol in score_sols}
            score_sols.sort(key=lambda x: x[0], reverse=True)
            sols = [x[1] for x in score_sols]
            sols = rouge_dedup(
                sols, args.lang, args.dedup_threshold, trim_top_comments=False)
            scores = [sols_to_score[sol] for sol in sols]
            edu_scores.extend(scores)
        elif args.strategy == "best":
            # best determined by edu score
            assert scorer is not None
            scores = scorer.score(sols)
            best, best_score = get_best_sol(scores, sols)
            sols = [best]
            edu_scores.append(best_score)
        elif args.strategy == "all":
            # keep all
            sols = all_sols
            edu_scores.extend([0] * len(sols))
        elif args.strategy == "random":
            # picks random one
            sol = random.choice(sols)
            sols = [sol]
            edu_scores.append(0)

    obj_sols: List[Solution] = []
    for sol, score in zip(sols, edu_scores):
        obj_sols.append(
            Solution(sol, score, original_id, pass_rate, func_tests, failing))

    return obj_sols, num_passed > 0


def process_dedup(tpl: Tuple[List[Solution], str, float]) -> List[Solution]:
    sols, lang, threshold = tpl
    code_to_sol = {}
    for sol in sols:
        code_to_sol[sol.code] = sol

    sols_code = list(code_to_sol.keys())
    sols_code = rouge_dedup(sols_code, lang, threshold)
    return [code_to_sol[sol] for sol in sols_code]


THREADS = os.cpu_count() - 1  # type: ignore
pool = multiprocessing.Pool(THREADS)
if args.no_threading:
    iter_size = len(list(make_path_iterator()))
    for path in tqdm(make_path_iterator(), total=iter_size):
        solns, has_at_least_one_passing = process_path(path)
        solutions.extend(solns)
        if has_at_least_one_passing:
            num_has_at_least_one_passing += 1
else:
    batch = []
    iter_size = len(list(make_path_iterator()))
    for i, path in tqdm(enumerate(make_path_iterator()), total=iter_size):
        batch.append(path)
        if len(batch) >= args.processing_batch or i == iter_size - 1:
            solns = pool.map(process_path, batch)
            for soln, has_at_least_one_passing in solns:
                solutions.extend(soln)
                if has_at_least_one_passing:
                    num_has_at_least_one_passing += 1
            batch = []


def compute_rounds(n, group_size, wanted_prob):
    p = (group_size - 1) / (n - 1)
    k = math.ceil(math.log(1 - wanted_prob) / math.log(1 - p))
    return k


if args.global_dedup:
    dedup_group_size = min(len(solutions), 200)
    dedup_rounds = compute_rounds(
        len(solutions), dedup_group_size, args.global_dedup_prob)
    prev_num_sols = len(solutions)
    for rnd in tqdm(range(dedup_rounds)):
        print(
            f" #### global dedup round {rnd+1}/{dedup_rounds}. current num solutions: {len(solutions)} ####")
        # shuffle solutions
        random.shuffle(solutions)
        groups = []
        for i in range(0, len(solutions), dedup_group_size):
            group = solutions[i: i + dedup_group_size]
            groups.append((group, args.lang, args.dedup_threshold))
        print(
            f"   # deduping {len(groups)} groups with {dedup_group_size} solutions each #")
        # dedup each group
        deduped_groups = pool.map(process_dedup, groups)
        # flatten
        solutions = []
        for group in deduped_groups:
            solutions.extend(group)

        dedup_group_size = min(len(solutions), 200)
        dedup_rate = 1 - (len(solutions) / prev_num_sols)
        print(
            f"   # dedup round {rnd+1} complete. current num solutions: {len(solutions)}. dedup rate: {dedup_rate} #"
        )
        prev_num_sols = len(solutions)


pool.close()
pool.join()

# score solutions (if dedup, otherwise we already have scores)
if args.strategy == "dedup" and not args.no_score:
    assert scorer is not None
    print(" #### scoring solutions #### ")
    def make_score_iterator(): return range(0, len(solutions), args.score_batch)
    for i in tqdm(make_score_iterator(), total=len(list(make_score_iterator()))):
        batch = solutions[i: i + args.score_batch]
        scores = scorer.score([sol.code for sol in batch])
        for sol, score in zip(batch, scores):
            sol.score = score


solution_codes = []
failing_codes = []
edu_scores = []
pass_rates = []
original_ids = []
tests = []

for sol in solutions:
    solution_codes.append(sol.code)
    failing_codes.append(sol.failing)
    edu_scores.append(sol.score)
    pass_rates.append(sol.pass_rate)
    original_ids.append(sol.original_id)
    tests.append(sol.tests)

new_ds = datasets.Dataset.from_dict(
    {
        "content": solution_codes,
        "failing_content": failing_codes,
        "pass_rate": pass_rates,
        "id": list(range(len(solutions))),
        "original_id": original_ids,
        "tests": tests,
        "edu_score": edu_scores
    })
new_ds.push_to_hub(args.name, private=True)

# stats
print(" #### stats #### ")
print(f"total solutions: {len(solutions)}")
print(f"total unique solutions: {len(set(solutions))}")
print(
    f"total problems with at least one solution: {num_has_at_least_one_passing}")
print(f"average edu score: {sum(edu_scores) / len(edu_scores)}")
print(f"average pass rate: {sum(pass_rates) / len(pass_rates)}")
