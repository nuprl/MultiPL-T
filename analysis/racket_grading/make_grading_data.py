import datasets
from glob import glob
from pathlib import Path
import random

def load_selections(model_dir):
    ds = []
    # for every subdir in model_dir
    for prob_dir in glob(f"{model_dir}/HumanEval*"):
        for file in glob(f"{prob_dir}/*.rkt"):
            if "selected" in file:
                program = "\n".join(open(file).readlines()[1:])
                ds.append({
                    "problem": prob_dir.split("/")[-1],
                    "program": program,
                })
                
    return datasets.Dataset.from_list(ds)

def merge_base_to_tuned_ds(base_ds, tune_ds, seed=42):
    # randomly assign a ds to model_A and the other to model_B
    random.seed(seed)
    model_A = random.choice([base_ds, tune_ds])
    model_B = base_ds if model_A == tune_ds else tune_ds
    
    with open("secret_key.md", "w") as f:
        # write which is which
        f.write(f"seed: {seed}\n")
        if model_A == base_ds:
            f.write("model_A: base_ds\n")
        else:
            f.write("model_A: tuned_ds\n")
    
    ds = []
    problems = set(model_A["problem"] + model_B["problem"])
    for p in problems:
        ds.append({
            "problem": p,
            "model_A": model_A["program"][model_A["problem"].index(p)],
            "model_B": model_B["program"][model_B["problem"].index(p)],
        })
    return datasets.Dataset.from_list(ds)
                
    
if __name__ == "__main__":
    base_ds = load_selections("select_for_grading/base_counterparts")
    tuned_ds = load_selections("select_for_grading/tuned")
    # some tuned problems are not in base; filter those out
    tuned_ds = tuned_ds.filter(lambda x: x["problem"] in base_ds["problem"])
    ds = merge_base_to_tuned_ds(base_ds, tuned_ds, seed=4)
    print(ds)
    ds.push_to_hub("franlucc/rkt-grading-15b")
    
    