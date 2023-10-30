import datasets
import numpy as np
import gzip 
import json 
from pathlib import Path
import argparse
import tarfile
import pandas as pd
# from glob import glob

def view_all_completions(json_gz_dir):
    s = ""
    for file in Path(json_gz_dir).glob("*.json.gz"):
        if "results" in file.name:
            continue
        data = gunzip_json(file)
        if data is None:
            continue
        programs = [data["prompt"] + "\n" + p for p in data["results"]["program"]]
        s += '\n\n'.join(programs) + "\n\n"
    return s

def remove_tests(dir):
    for file in Path(dir).glob("*.rkt"):
        with open(file, "r") as f:
            lines = f.readlines()
        with open(file, "w") as f:
            for line in lines:
                if "(require rackunit)" in line:
                    break
                else:
                    f.write(line)
        
def pass_k(n: int, c: int, k: int) -> float:
    """
    Calculates 1 - comb(n - c, k) / comb(n, k).
    English: The probability that a program would pass tests in k attempts given n 
    attempts cases and c passes.

    Precondition: n >>> k
    """
    if n - c < k:
        return 1.0
    return 1.0 - np.prod(1.0 - k / np.arange(n - c + 1, n + 1))

def proc_multiple_results(file):
    data = gunzip_json(file)
    if data is None:
        return None
    programs = []
    statuses = []
    for res in data["results"]:
        statuses.append(res["status"])
        programs.append(res["program"].split("(require rackunit)")[0].strip())
    return data["name"], statuses,  programs

def gunzip_json(path): 
    """
    Reads a .json.gz file, but produces None if any error occurs.
    """
    try:
        with gzip.open(path, "rt") as f:
            return json.load(f)
    except Exception as e:
        print(f"Error reading {path}: {e}")
        return None

def multiple_results_to_ds(path):
    ds_dict = {
        "problem": [],
        "statuses": [],
        "programs": [],
    }
    for file in path.glob("*.results.json.gz"):
        res = proc_multiple_results(file)
        if res is None:
            continue
        name, statuses, programs = res
        ds_dict["problem"].append(name)
        ds_dict["statuses"].append(statuses)
        ds_dict["programs"].append(programs)
    return datasets.Dataset.from_dict(ds_dict)

def filter_successful_programs(ds):
    return ds.filter(lambda x: any(s.upper() == "OK" for s in x["statuses"]))

def extract_successful(results_dir, max_completions=20):
    """Selects first N completions per problem and returns a dataset of successful programs."""
    ds = multiple_results_to_ds(Path(results_dir))
    ds = select(ds, max_completions)
    ds = filter_successful_programs(ds)
    return ds

def select(ds, num_completions):
    """Selects first num_completions completions per problem."""
    ds = ds.map(lambda x: {"programs": x["programs"][:num_completions], "statuses": x["statuses"][:num_completions]})
    return ds

def make_scoresheet_template(n):
    """
    Makes a scoresheet template for n problems.
    """
    scoresheet = []
    for i in range(n):
        scoresheet.append({
            "File" : f"prog_{i}.rkt",
            "[TEXT] Dangling Parens" : -0.5,
            "[TEXT] Line too long" : -1,
            "[TEXT] Using car cdr" : -0.5,
            "[TEXT] Cond without square brackets" : -0.5,
            "[TEXT] Uninformative local names" : -0.5,
            "[DEFNS] let-expr not at topmost start" : -1,
            "[DEFNS] Nesting let-exprs or defines" : -2,
            "[DEFNS] Unnecessary let* or letrec exprs" : -1,
            "[DEFNS] Unused or useless local vars" : -1,
            "[DEFNS] Not defining helpers or local vars" : -1,
            "[COND] Nested if-expr instead of cond": -2,
            "[COND] (if COND #t #f)":-1,
            "[TRAV] iteration instead of recursion" : -3,
        })
    pd.DataFrame(scoresheet).to_csv("grader_scoresheet.csv", index=False)
    
if __name__ == "__main__":
    # parser = argparse.ArgumentParser()
    # parser.add_argument("path", type=Path, help="Path to a dir containing .results.json.gz files.")
    # args = parser.parse_args()
    # d = extract_successful(args.path)
    # print(d["programs"])
    make_scoresheet_template(70)

