import pandas as pd
import json
import re
import glob
import os
import gzip

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

def get_stats(df):
    """
    Create a df with column names: problem_name, base_model_score, tuned_model_score,
    """
    def file_to_humaneval(filename, grading_dir="rename_grading_dir"):
        def get_name(filename, names):
            for name in names:
                if filename.replace(".rkt","_") in name:
                    return "HumanEval" + name.split("HumanEval")[-1].split(".rkt")[0]
            raise Exception(f"Could not find {filename} in {grading_dir}")
        
        names = glob.glob(os.path.join(grading_dir, "*"))
        return get_name(filename, names)
    
    df["problem"] = df["File"].apply(file_to_humaneval)
    
    # return a df with columns "problem", "base_model_score", "tuned_model_score"
    df = df[["problem", "model", "total score"]]
    df = df.groupby(["problem", "model"]).mean().reset_index()
    df = df.pivot(index="problem", columns="model", values="total score")
    df = df.reset_index()
    df = df.rename(columns={"base":"base_model_score", "tuned":"tuned_model_score"})
    
    # add a row for total score
    df.loc[len(df)] = ["total", df["base_model_score"].sum(), df["tuned_model_score"].sum()]
    
    return df
    


if __name__ == "__main__":
    df = load_grader_scores("fran_final_grader_scoresheet.csv")
    df = deanonimize(df, "secret_key_hash.json")
    df = get_stats(df)
    df.to_csv("fran_grading_stats.csv", index=False)
    
    # # read json.gz file
    # sanity check
    # with gzip.open("humaneval-rkt-15b-tuned/HumanEval_9_rolling_max.results.json.gz", "r") as f:
    #     print(json.load(f)["results"][4]["program"])
    # with gzip.open("../inspect_completions/starcoderbase-15b-results-rkt/HumanEval_9_rolling_max.results.json.gz", "r") as f:
    #     print(json.load(f)["results"][0]["program"])
    
