import datasets
import json
import gzip
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
pa.add_argument("--lang", type=str, required=True)
pa.add_argument("--dedup_threshold", type=float, default=0.6)
pa.add_argument("--score_batch", type=int, default=32)
pa.add_argument("--score_device", type=str, default="cpu")
args = pa.parse_args()

possible_strategies = ["dedup", "dedup_best", "best"]
if args.strategy not in possible_strategies:
    assert False, f"invalid strategy {args.strategy}, must be one of {possible_strategies}"

solutions = []
edu_scores = []
original_ids = []
pass_rates = []
tests = []

scorer = CodeScorer("nuprl/code-scorer-edu-v1", device=args.score_device)


def get_best_sol(scores, sols):
    max_score = 0
    best_sol_idx = 0
    for i, score in enumerate(scores):
        if score > max_score:
            max_score = score
            best_sol_idx = i

    return sols[best_sol_idx], max_score


for path in progressbar(Path(args.path).glob("**/*.results.json.gz")):
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

    pass_rate = num_passed / (num_passed + num_failed)

    # simple set dedup
    sols = list(set(sols))

    if len(sols) > 0:
        if args.strategy == "dedup":
            sols = dedup(sols, args.lang, args.dedup_threshold)
        elif args.strategy == "dedup_best":
            # IDEA: sort the solutions by score, then dedup, so higher scoring solutions are more likely to be kept
            scores = scorer.score(sols)
            score_sols = list(zip(scores, sols))
            score_sols.sort(key=lambda x: x[0], reverse=True)
            scores = [x[0] for x in score_sols]
            sols = [x[1] for x in score_sols]
            sols = dedup(sols, args.lang, args.dedup_threshold)
            edu_scores.extend(scores)
        elif args.strategy == "best":
            # best determined by edu score
            scores = scorer.score(sols)
            best, best_score = get_best_sol(scores, sols)
            sols = [best]
            edu_scores.append(best_score)

    solutions.extend(sols)
    pass_rates.extend([pass_rate] * len(sols))
    original_ids.extend([original_id] * len(sols))
    tests.extend([func_tests] * len(sols))

# score solutions (if dedup, otherwise we already have scores)
if args.strategy == "dedup":
    print(" #### scoring solutions #### ")
    for i in progressbar(range(0, len(solutions), args.score_batch)):
        batch = solutions[i: i + args.score_batch]
        scores = scorer.score(batch)
        edu_scores.extend(scores)


new_ds = datasets.Dataset.from_dict(
    {"content": solutions, "pass_rate": pass_rates, "id": list(range(len(solutions))), "original_id": original_ids, "tests": tests, "edu_score": edu_scores})
new_ds.push_to_hub(args.name)

# stats
print(" #### stats #### ")
print(f"total solutions: {len(solutions)}")
print(f"average edu score: {sum(edu_scores) / len(edu_scores)}")
print(f"average pass rate: {sum(pass_rates) / len(pass_rates)}")
