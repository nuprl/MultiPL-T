import datasets 
import pandas as pd
import argparse
from pathlib import Path
import numpy as np

def pass_k(n: int, c: int, k: int) -> float:
    """
    Calculates 1 - comb(n - c, k) / comb(n, k).
    """
    if n - c < k:
        return 1.0
    return 1.0 - np.prod(1.0 - k / np.arange(n - c + 1, n + 1))

def parse_experiment_name(name):
    splitname = name.split("-")
    data = splitname[0]
    lang = splitname[1]
    model = splitname[2]
    temp = splitname[3]
    config = splitname[4]
    return data, lang, model, temp, config

def test_experiment_name(name, bench, model, temp, config):
    b, l, m, t, c = parse_experiment_name(name)
    return b == bench and m == model and t == temp and c == config

def add_success_ratio(ds):
    statuses = ds["statuses"]
    sr = statuses.count("OK") / len(statuses)
    ds["success_ratio"] = sr
    return ds

def parse_problem_name(name):
    splitname = name.split("_")
    number = int(splitname[1])
    pname = "_".join(splitname[2:])
    return number, pname

def get_problem_number(name):
    number, _ = parse_problem_name(name)
    return number

def trim_problem_name(name):
    number, pname = parse_problem_name(name)
    return f"{number}_{pname}"

def remap_language(lang):
    if lang == "cpp":
        return "C++"
    elif lang == "cs":
        return "C#"
    elif lang == "d":
        return "D"
    elif lang == "go_test.go":
        return "Go"
    elif lang == "java":
        return "Java"
    elif lang == "jl":
        return "Julia"
    elif lang == "js":
        return "JavaScript"
    elif lang == "lua":
        return "Lua"
    elif lang == "php":
        return "PHP"
    elif lang == "pl":
        return "Perl"
    elif lang == "py":
        return "Python"
    elif lang == "r":
        return "R"
    elif lang == "rb":
        return "Ruby"
    elif lang == "rkt":
        return "Racket"
    elif lang == "rs":
        return "Rust"
    elif lang == "scala":
        return "Scala"
    elif lang == "sh":
        return "Shell"
    elif lang == "swift":
        return "Swift"
    elif lang == "ts":
        return "TypeScript"
    elif lang == "m":
        return "MATLAB"
    else:
        raise ValueError(f"Unknown language {lang}")
        

def proc_dataset(raw_ds, bench, model, temp, config, languages=None):
    if languages  is not None:
        languages = languages.split(",")
    proc_ds = raw_ds.filter(lambda x: test_experiment_name(x["experiment"], bench, model, temp, config))
    proc_ds = proc_ds.map(add_success_ratio)
    proc_ds = proc_ds.map(lambda x: {"problem" : trim_problem_name(x["problem"]),
                                     "language" : remap_language(x["language"]) } 
                         )
    valid = [v for (_, v) in proc_ds.items() if v.num_rows > 0]
    prob_dict = {}
    for ds in valid:
        problems = ds["problem"]
        langs = ds["language"]
        if languages is not None:
            if langs[0] not in languages:
                continue
        success_ratios = ds["success_ratio"]
        for (prob, lang, sr) in zip(problems, langs, success_ratios):
            if prob not in prob_dict:
                prob_dict[prob] = {}
            prob_dict[prob][lang] = sr
    raw_table = []
    for (prob, lang_dict) in prob_dict.items():
        for (lang, sr) in lang_dict.items():
            raw_table.append([prob, lang, sr])
    return pd.DataFrame(raw_table, columns=["problem", "language", "success_ratio"])       



if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--bench", type=str, required=True, help="The benchmark to process (humaneval or mbpp)")
    parser.add_argument("--model", type=str, required=True, help="The model to process")
    parser.add_argument("--temp", type=str, required=True, help="The temperature to process")
    parser.add_argument("--config", type=str, required=True, help="The config to process (e.g. reworded)")
    parser.add_argument("--languages", type=str, default=None, help="The languages to process, comman separated")
    parser.add_argument("--output-csv", type=str, required=True, help="The output file to write to")

    args = parser.parse_args()
    raw_ds = datasets.load_dataset("bigcode/MultiPL-E-completions")
    print(raw_ds)
    df = proc_dataset(raw_ds, args.bench, args.model, args.temp, args.config)
    df.to_csv(args.output_csv, index=False)
    print(f"Processed dataset written to {args.output_csv}")

