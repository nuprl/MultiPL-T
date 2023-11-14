from glob import glob
import re
import pandas as pd
import os

cols_order = ["Problem","Model","grader",
    "[TEXT] Dangling Parens",
    "[TEXT] Line too long",
    "[TEXT] Using car cdr",
    "[TEXT] Cond without square brackets",
    "[TEXT] Uninformative local names",
    "[DEFNS] let-expr not at topmost start",
    "[DEFNS] Nesting let-exprs or defines",
    "[DEFNS] Unnecessary let* or letrec exprs",
    "[DEFNS] Unused or useless local vars",
    "[DEFNS] Not defining helpers or local vars",
    "[COND] Nested if-expr instead of cond",
    "[COND] (if COND #t #f)",
    "[TRAV] iteration instead of recursion",
    "File"]

file_id_to_humaneval_id = {}
file_id_to_model = {}

for model in ["tuned", "base"]:
    for file in glob(f"../deanon_renamed_grading_dir/{model}/*.rkt"):
        humaneval_id = "HumaneEval" + file.split("HumanEval")[-1].replace(".rkt", "")
        fname = os.path.basename(file).split("HumanEval")[0][:-1] + ".rkt"
        file_id_to_humaneval_id[fname] = humaneval_id
        file_id_to_model[fname] = model


def anon_to_humaneval(df, grader_id):
    df["Problem"] = df["File"].apply(lambda x: file_id_to_humaneval_id[x])
    df["Model"] = df["File"].apply(lambda x: file_id_to_model[x])
    df["grader"] = df["File"].apply(lambda x: grader_id)
    df = df[cols_order]
    # rename "file" to "anon_id"
    df = df.rename(columns={"File": "anon_id"})
    return df

grader1 = "fran_final_grader_scoresheet.csv"
grader2 = "grader_scoresheet_gouwar.csv"
df1 = pd.read_csv(grader1)
df2 = pd.read_csv(grader2)
df1 = anon_to_humaneval(df1, "grader_0")
df2 = anon_to_humaneval(df2, "grader_1")


with open(f"deanon_{grader1}", "w") as f:
    df1.to_csv(f, index=False)
with open(f"deanon_{grader2}", "w") as f:
    df2.to_csv(f, index=False)
    
df3 = pd.concat([df1, df2])
# order by problem, base first
df3 = df3.sort_values(by=["Problem", "Model", "grader"])
df3 = df3.reset_index(drop=True)
with open(f"full_scores.csv", "w") as f:
    df3.to_csv(f, index=False)