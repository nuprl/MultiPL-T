import datasets
from glob import glob
from pathlib import Path
import random
from hashlib import blake2b
import os
import json

def load_selections(model_dir):
    ds = []
    # for every subdir in model_dir
    for prob_dir in glob(f"{model_dir}/HumanEval*"):
        for file in glob(f"{prob_dir}/*.rkt"):
            if "selected" in file:
                program = "".join(open(file).readlines()[1:])
                ds.append({
                    "problem": prob_dir.split("/")[-1],
                    "program": program,
                })
                
    return datasets.Dataset.from_list(ds)

def merge_base_to_tuned_ds(base_ds, tune_ds, seed=42):
    
    def random_model(seed):
        # randomly assign a ds to model_A and the other to model_B
        random.seed(seed)
        model_A = random.choice([base_ds, tune_ds])
        model_B = base_ds if model_A == tune_ds else tune_ds
        secret_key = {"model_B" : "base" if model_B == base_ds else "tuned",
               "model_A" : "base" if model_A == base_ds else "tuned",
               "seed": seed}
        return model_A, model_B, secret_key
    
    secret_keys = []
    ds = []
    problems = base_ds["problem"]
    for i,p in enumerate(problems):
        model_A, model_B, secret_key = random_model(seed)
        secret_key.update({"problem": p, "id":i})
        secret_keys.append(secret_key)
        ds.append({
            "problem": p,
            "id": i,
            "model_A": model_A["program"][model_A["problem"].index(p)],
            "model_B": model_B["program"][model_B["problem"].index(p)],
        })
        seed += 1
        
    with open(f"pair_secret_key.json", "w") as f:
        json.dump(secret_keys, f, indent=2)
    return ds

def make_pair_grading_dir(base_ds, tune_ds, outdir, seed=42):
    """Makes a grading dir for comparing pairs"""
    if os.path.exists(outdir):
        raise ValueError(f"{outdir} already exists.")
    os.makedirs(outdir)
    
    merged_list = merge_base_to_tuned_ds(base_ds, tune_ds, seed=seed)
    
    for pair_dict in merged_list:
        problem = pair_dict["problem"]
        id = pair_dict["id"]
        with open(f"{outdir}/prog_{id}_{problem}.rkt", "w") as f:
            f.write(";; Example A\n")
            f.write(str(pair_dict["model_A"]))
            f.write("\n;; Example B\n")
            f.write(str(pair_dict["model_B"]))
    

def make_grading_dir(base_ds, tune_ds, outdir, seed=42):
    """Makes a grading dir for comparing pairs
    Uses hash names initially and renames to int ids so that grader
    cannot reverse engineer order of problems from ids."""
    
    hashes = {"base":[], "tuned":[]}
    if os.path.exists(outdir):
        raise ValueError(f"{outdir} already exists.")
    os.makedirs(outdir)
    
    sha = blake2b(digest_size=10)
    def write_programs(ds, model_name):
        for i in range(len(ds)):
            sha.update(ds["program"][i].encode("utf-8"))
            h = sha.hexdigest()
            hashes[model_name].append(h)
            with open(f"{outdir}/prog_{h}.rkt", "w") as f:
                f.write(str(ds["program"][i]))
            
    # assign unique hash to problems in tuned and base
    write_programs(tune_ds, "tuned")
    write_programs(base_ds, "base")
    assert len(set(hashes["base"] + hashes["tuned"])) == len(base_ds) + len(tune_ds)
    
    # shuffle and give readable ids
    random.seed(seed)
    all_hashes = list(set(hashes["base"] + hashes["tuned"]))
    random.shuffle(all_hashes)
    hash_to_id = {h: i for i, h in enumerate(all_hashes)}
    
    # rename files in dir to ids
    for h in all_hashes:
        os.rename(f"{outdir}/prog_{h}.rkt", f"{outdir}/prog_{hash_to_id[h]}.rkt")
        
    with open(f"secret_key_hash.json", "w") as f:
        hashes = {
            "seed": seed,
            "base": [hash_to_id[h] for h in hashes["base"]],
            "tuned": [hash_to_id[h] for h in hashes["tuned"]],
        }
        json.dump(hashes, f, indent=2)
        
    
if __name__ == "__main__":
    base_ds = load_selections("select_for_grading/base_counterparts")
    tuned_ds = load_selections("select_for_grading/tuned")
    # some tuned problems are not in base; filter those out
    tuned_ds = tuned_ds.filter(lambda x: x["problem"] in base_ds["problem"])

    # make_grading_dir(base_ds, tuned_ds, "grading_dir", seed=random.randint(0, 1000))
    make_pair_grading_dir(base_ds, tuned_ds, "pair_grading_dir", seed=41)
    
    