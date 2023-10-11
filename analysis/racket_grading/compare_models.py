import datasets
import argparse
from utils import extract_successful, multiple_results_to_ds, select
from pathlib import Path
import json
import os

def process_row(row):
    """
    Returns a new row with only successful programs.
    """
    new_row = {"problem": row["problem"], 
               "statuses": [], 
               "programs": []}
    
    for i in range(len(row["programs"])):
        if row["statuses"][i].upper() == "OK":
            new_row["statuses"].append(row["statuses"][i])
            new_row["programs"].append(row["programs"][i])
    return new_row

def save_subdir(ds, subdir):
    os.makedirs(subdir, exist_ok=True)
    for i in range(len(ds)):
        problem = ds["problem"][i]
        os.makedirs(f"{subdir}/{problem}", exist_ok=True)
        for j in range(len(ds["programs"][i])):
            program = ds["programs"][i][j]
            with open(f"{subdir}/{problem}/program_{j}.rkt", "w") as f:
                f.write(program)
                
def save_for_hand_selection(base_counterparts, tuned):
    """
    Saves as dir:
        select_for_grading/
            ├── base_counterparts/
            │   ├── {problem1_name}
            │   │    ├── program_1.rkt
            │   │    ├── program_2.rkt
            │   │    └── ...
            │   ├── {problem2_name}
            │        ├── program_1.rkt
            │        └── ...
            ├── tuned/
                ├── {problem1_name}
                │    ├── program_1.rkt
                │    ├── program_2.rkt
                │    └── ...
                ├── {problem2_name}
                     ├── program_1.rkt
                     └── ...
    Then once a problem is selected by hand, rename it selected_program_{n}.rkt
    """
    base_counterparts = base_counterparts.map(process_row)
    tuned = tuned.map(process_row)
    # create dir
    os.makedirs("select_for_grading", exist_ok=True)
    save_subdir(base_counterparts, "select_for_grading/base_counterparts")
    save_subdir(tuned, "select_for_grading/tuned")
    

        
def compare(path_base, path_tuned):
    ds_base = extract_successful(path_base, 20)
    ds_tuned = extract_successful(path_tuned, 20)
    base = path_base.name
    tuned = path_tuned.name
    
    # num completions per problem
    print(f"Num completions per problem in base: {len(ds_base['programs'][0])}")
    print(f"Num completions per problem in tuned: {len(ds_tuned['programs'][0])}")
    print(f"Num successful base: {len(ds_base)}")
    print(f"Num successful tuned: {len(ds_tuned)}")
    
    problems_tuned = set(ds_tuned["problem"])
    problems_base = set(ds_base["problem"])
    # problems in tuned that are not in base
    print(f"Problems in tuned that are not in base: {problems_tuned - problems_base}")
    # problems in base that are not in tuned
    print(f"Problems in base that are not in tuned: {problems_base - problems_tuned}")
    
    # base counterparts of successful problems in tuned
    base_all = select(multiple_results_to_ds(path_base), 20)
    base_counterparts = base_all.filter(lambda x: x["problem"] in problems_tuned)
    assert base_counterparts["problem"] == ds_tuned["problem"]
    
    # save for hand-selection
    save_for_hand_selection(base_counterparts, ds_tuned)
    
    

def main(args):
    compare(args.baseline, args.tuned)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("baseline", type=Path, help="Path to a dir containing .results.json.gz files for baseline model.")
    parser.add_argument("tuned", type=Path, help="Path to a dir containing .results.json.gz files for tuned model.")
    args = parser.parse_args()
    main(args)