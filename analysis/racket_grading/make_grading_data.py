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
    # randomly assign a ds to model_A and the other to model_B
    random.seed(seed)
    model_A = random.choice([base_ds, tune_ds])
    model_B = base_ds if model_A == tune_ds else tune_ds
    
    with open(f"secret_key_{seed}.md", "w") as f:
        # write which is which
        f.write(f"seed: {seed}\n")
        if model_A == base_ds:
            f.write("model_A: base_ds\n")
        else:
            f.write("model_A: tuned_ds\n")
    
    ds = []
    problems = model_A["problem"]
    for p in problems:
        ds.append({
            "problem": p,
            "model_A": model_A["program"][model_A["problem"].index(p)],
            "model_B": model_B["program"][model_B["problem"].index(p)],
        })
    return datasets.Dataset.from_list(ds)

def make_grading_dir(base_ds, tune_ds, outdir, seed=42):
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
    # ds = merge_base_to_tuned_ds(base_ds, tuned_ds, seed=random.randint(0, 1000))
    # print(ds)
    make_grading_dir(base_ds, tuned_ds, "grading_dir", seed=random.randint(0, 1000))
    
    