from typing import List
import datasets
import json
import gzip
import multiprocessing
from progressbar import progressbar
from pathlib import Path
from dedup_solutions import dedup
from code_scorer.inference import CodeScorer
from utils import clean_sol_prompt
from argparse import ArgumentParser

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
pa.add_argument("--strategy", type=str, default="dedup")
pa.add_argument("--global_dedup", action="store_true")
pa.add_argument("--lang", type=str, required=True)
pa.add_argument("--dedup_threshold", type=float, default=0.6)
pa.add_argument("--score_batch", type=int, default=32)
pa.add_argument("--processing_batch", type=int, default=512)
pa.add_argument("--score_device", type=str, default="cpu")
pa.add_argument("--no_score", action="store_true")
args = pa.parse_args()

possible_strategies = ["dedup", "dedup_best", "best"]
if args.strategy not in possible_strategies:
    assert False, f"invalid strategy {args.strategy}, must be one of {possible_strategies}"

num_has_at_least_one_passing = 0


class Solution:
    def __init__(self, code, score, original_id, pass_rate, tests):
        self.code = code
        self.score = score
        self.original_id = original_id
        self.pass_rate = pass_rate
        self.tests = tests


solutions: List[Solution] = []

scorer = None
if not args.no_score:
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


def process_path(path):
    global num_has_at_least_one_passing
    global solutions
    global scorer
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    original_id = int(path.stem.split("_")[1])
    results = data["results"]
    func_tests = data["tests"]

    sols = []
    num_failed = 0
    num_passed = 0
    for res in results:
        if res["exit_code"] == 0:
            num_passed += 1
            sol = clean_sol_prompt(args.lang, res["program"])
            if "TODO" in sol:
                continue
            sols.append(sol)
        else:
            num_failed += 1

    if num_passed > 0:
        num_has_at_least_one_passing += 1

    pass_rate = num_passed / (num_passed + num_failed)

    # simple set dedup
    sols = list(set(sols))

    edu_scores = []
    if len(sols) > 0:
        if args.strategy == "dedup":
            sols = dedup(sols, args.lang, args.dedup_threshold)
            edu_scores.extend([0] * len(sols))
        elif args.strategy == "dedup_best":
            # IDEA: sort the solutions by score, then dedup, so higher scoring solutions are more likely to be kept
            assert scorer is not None
            scores = scorer.score(sols)
            score_sols = list(zip(scores, sols))
            sols_to_score = {sol: score for score, sol in score_sols}
            score_sols.sort(key=lambda x: x[0], reverse=True)
            sols = [x[1] for x in score_sols]
            sols = dedup(sols, args.lang, args.dedup_threshold)
            scores = [sols_to_score[sol] for sol in sols]
            edu_scores.extend(scores)
        elif args.strategy == "best":
            # best determined by edu score
            assert scorer is not None
            scores = scorer.score(sols)
            best, best_score = get_best_sol(scores, sols)
            sols = [best]
            edu_scores.append(best_score)

    obj_sols = []
    for sol, score in zip(sols, edu_scores):
        obj_sols.append(
            Solution(sol, score, original_id, pass_rate, func_tests))

    solutions.extend(obj_sols)


with multiprocessing.Pool() as pool:
    batch = []
    iter_size = len(list(make_path_iterator()))
    for i, path in progressbar(enumerate(make_path_iterator()), max_value=iter_size):
        batch.append(path)
        if len(batch) >= args.processing_batch or i == iter_size - 1:
            pool.map(process_path, batch)
            batch = []

# score solutions (if dedup, otherwise we already have scores)
if args.strategy == "dedup" and not args.no_score:
    assert scorer is not None
    print(" #### scoring solutions #### ")
    def make_score_iterator(): return range(0, len(solutions), args.score_batch)
    for i in progressbar(make_score_iterator(), max_value=len(list(make_score_iterator()))):
        batch = solutions[i: i + args.score_batch]
        scores = scorer.score([sol.code for sol in batch])
        for sol, score in zip(batch, scores):
            sol.score = score


solution_codes = []
edu_scores = []
pass_rates = []
original_ids = []
tests = []

for sol in solutions:
    solution_codes.append(sol.code)
    edu_scores.append(sol.score)
    pass_rates.append(sol.pass_rate)
    original_ids.append(sol.original_id)
    tests.append(sol.tests)

new_ds = datasets.Dataset.from_dict(
    {
        "content": solution_codes,
        "pass_rate": pass_rates,
        "id": list(range(len(solutions))),
        "original_id": original_ids,
        "tests": tests,
        "edu_score": edu_scores
    })
new_ds.push_to_hub(args.name)

# stats
print(" #### stats #### ")
print(f"total solutions: {len(solutions)}")
print(f"total unique solutions: {len(set(solutions))}")
print(
    f"total problems with at least one solution: {num_has_at_least_one_passing}")
print(f"average edu score: {sum(edu_scores) / len(edu_scores)}")
print(f"average pass rate: {sum(pass_rates) / len(pass_rates)}")
