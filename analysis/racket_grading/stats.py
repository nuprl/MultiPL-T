import pandas as pd
import json
import re
import os

def load_grader_scores(csv_file, max_score=15):
    df = pd.read_csv(csv_file)
    # add a column that adds up all columns except for "File"
    df["deduction"] = df.drop("File", axis=1).sum(axis=1)
    df["total score"] = max_score - df["deduction"]
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
    Create a df with column names: problem_name, model, total score
    """
    def file_to_humaneval(filename):
        return "HumanEval" + filename.split("HumanEval")[-1].split(".rkt")[0].strip()
    
    df["problem"] = df["File"].apply(file_to_humaneval)
    return df[["problem", "model", "total score"]]
