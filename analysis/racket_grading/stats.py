import pandas as pd
import json
import re
import glob
import os
import gzip
import argparse

def load_grader_scores(csv_file, max_score=15):
    df = pd.read_csv(csv_file)
    # add a column that adds up all columns except for "File"
    df["deduction"] = df.drop("File", axis=1).sum(axis=1)
    df["total score"] = max_score
    # apply deduction
    df["total score"] += df["deduction"]
    # make total score int
    df["total score"] = df["total score"].astype(int)
    return df

def load_pair_scores(csv_file):
    df = pd.read_csv(csv_file)
    return df.to_dict(orient="records")
    
def deanonimize_pairs(pair_list, secret_key_file):
    key_list = json.load(open(secret_key_file, "r"))
    keys = {d["id"]:d for d in key_list}
    deanon_pairs = []
    for pair in pair_list:
        id = int(pair["File"].split("_")[-1].split(".rkt")[0].strip())
        model_A = ("tuned" if keys[id]["model_A"] == "tuned" else "base")
        model_B = ("tuned" if keys[id]["model_B"] == "tuned" else "base")
        deanon_pairs.append({"file":pair["File"], model_A:pair["Example A"], model_B:pair["Example B"]})
        
    df = pd.DataFrame(deanon_pairs)
    # add a ROW for total of column "tuned" and "base"
    df.loc[len(df)] = ["total", df["tuned"].sum(), df["base"].sum()]
    return df
    

def deanonimize(df, secret_key_file):
    keys = json.load(open(secret_key_file, "r"))
    
    id_to_model = {}
    for k in ["base", "tuned"]:
        for id in keys[k]:
            id_to_model[id] = k
    
    def get_id(filename):
        return int(filename.split("_")[-1].split(".rkt")[0].strip())
    
    # for each row in df, add a column "model" that is the model name
    df["model"] = df["File"].apply(lambda x: id_to_model[get_id(x)])
    
    return df

def file_to_humaneval(filename, grading_dir="rename_grading_dir"):
    """
    Rename filenames to match the human eval filenames
    """
    def get_name(filename, names):
        for name in names:
            if filename.replace(".rkt","") == name.split("/")[-1].split("_HumanEval")[0]:
                return "HumanEval" + name.split("HumanEval")[-1].split(".rkt")[0]
        raise Exception(f"Could not find {filename} in {grading_dir}")
    
    names = glob.glob(f"{grading_dir}/*.rkt")
    return get_name(filename, names)
    
def get_stats(df):
    """
    Create a df with column names: problem_name, base_model_score, tuned_model_score,
    """
    df["problem"] = df["File"].apply(file_to_humaneval)
    
    df = df[["problem", "model", "total score"]]
    df = df.groupby(["problem", "model"]).sum().reset_index()
    df = df.pivot(index="problem", columns="model", values="total score")
    df = df.reset_index()
    df = df.rename(columns={"base":"base_model_score", "tuned":"tuned_model_score"})
    
    # add a row for total score
    df.loc[len(df)] = ["total", df["base_model_score"].sum(), df["tuned_model_score"].sum()]
    
    return df
    
    
def main(args):
    if args.do_pair:
        d = load_pair_scores(args.input)
        df = deanonimize_pairs(d, args.secret_key)
        df.to_csv(args.output, index=False)
    else:
        df = load_grader_scores(args.input)
        df = deanonimize(df, args.secret_key)
        df = get_stats(df)
        df.to_csv(args.output, index=False)
    

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("input", help="csv file with grader scores")
    parser.add_argument("output", help="output csv file")
    parser.add_argument("secret_key", help="json file with secret key")
    parser.add_argument("--do-pair", action="store_true", help="if true, do pair stats")
    args = parser.parse_args()
    main(args)
    
    # # read json.gz file
    # sanity check
    # with gzip.open("humaneval-rkt-15b-tuned/HumanEval_9_rolling_max.results.json.gz", "r") as f:
    #     print(json.load(f)["results"][4]["program"])
    # with gzip.open("../inspect_completions/starcoderbase-15b-results-rkt/HumanEval_9_rolling_max.results.json.gz", "r") as f:
    #     print(json.load(f)["results"][0]["program"])
    
