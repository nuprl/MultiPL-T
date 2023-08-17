import datasets 
from datasets import DatasetDict
import argparse
import numpy as np



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


def proc_dataset(raw_ds: datasets.DatasetDict, bench, model, temp, config, languages=None):
    proc_ds = raw_ds.filter(lambda x: test_experiment_name(x["experiment"], bench, model, temp, config))
    proc_ds = proc_ds.map(add_success_ratio)
    proc_ds = proc_ds.map(lambda x: {"problem" : trim_problem_name(x["problem"]) })
    valid = [(k, v) for (k, v) in proc_ds.items() if v.num_rows > 0]
    all_ds = []
    for (k, ds) in valid: 
        problems = ds["problem"]
        langs = ds["language"]
        statuses = ds["statuses"]
        completions = ds["completions"]
        prompts = ds["prompt"]
        nds = datasets.Dataset.from_dict({ "problem": problems, "language": langs, "status" : statuses, "completion" : completions, "prompt" : prompts})
        all_ds.append(nds)
    return datasets.concatenate_datasets(all_ds)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--bench", type=str, required=True, help="The benchmark to process (humaneval or mbpp)")
    parser.add_argument("--model", type=str, required=True, help="The model to process")
    parser.add_argument("--temp", type=str, required=True, help="The temperature to process")
    parser.add_argument("--config", type=str, required=True, help="The config to process (e.g. reworded)")

    args = parser.parse_args()
    raw_ds = datasets.load_dataset("bigcode/MultiPL-E-completions")
    final_ds = proc_dataset(raw_ds, args.bench, args.model, args.temp, args.config)
    final_ds.to_json("starcoderbase-15b-results.jsonl")
    print("Done!")