import datasets
import numpy as np
import gzip 
import json 
from pathlib import Path

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
        programs.append(res["program"])
    return data["name"], statuses,  programs

def gunzip_json(path): 
    """
    Reads a .json.gz file, but produces None if any error occurs.
    """
    try:
        with gzip.open(path, "rt") as f:
            return json.load(f)
    except Exception as e:
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

if __name__ == "__main__":
    ds = multiple_results_to_ds(Path("humaneval-rkt-15b-tuned"))
    ds = filter_successful_programs(ds)
    print(ds)

