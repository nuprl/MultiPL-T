import datasets
import re
import json
import gzip
from pathlib import Path
from argparse import ArgumentParser
from progressbar import progressbar
from utils import clean_sol_prompt

pa = ArgumentParser()
pa.add_argument("--path", type=str, required=True)
pa.add_argument("--name", type=str, required=True)
pa.add_argument("--lang", type=str, required=True)
pa.add_argument("--min_tests", type=int, default=3)
args = pa.parse_args()

funcs_pass = []
funcs_fail = []
funcs_fail_messages = []
pass_rates = []
tests = []
num_tests = []
ids = []


def make_path_iterator(): return Path(args.path).glob("**/*.results.json.gz")


RE_DIGITS = re.compile(r"\d+")
for path in progressbar(make_path_iterator(), max_value=len(list(make_path_iterator()))):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    # get first number in path.stem
    re_result = RE_DIGITS.search(path.stem)
    assert re_result is not None, f"Could not find number in {path.stem}"
    e_id = int(re_result.group(0))
    results = data["results"]
    tests_code = data["tests"]

    passed = []
    failed = []
    failed_messages = []

    for res in results:
        sol = clean_sol_prompt(args.lang, res["program"])
        if res["exit_code"] == 0:
            passed.append(sol)
        else:
            failed.append(sol)
            failed_messages.append(res["stderr"] + res["stdout"])

    if args.lang == "lua":
        _num_tests = tests_code.count("lu.assertEquals")
    elif args.lang == "py" or args.lang == "luau":
        _num_tests = tests_code.count("assert")
    else:
        raise NotImplementedError(f"Language {args.lang} not implemented")

    num_tests.append(_num_tests)
    tests.append(tests_code)
    pass_rates.append(len(passed) / len(results))
    funcs_pass.append(passed)
    funcs_fail.append(failed)
    funcs_fail_messages.append(failed_messages)
    ids.append(e_id)

new_ds = datasets.Dataset.from_dict(
    {
        "id": ids,
        "num_tests": num_tests,
        "tests": tests,
        "pass_rate": pass_rates,
        "pass": funcs_pass,
        "fail": funcs_fail,
        "fail_message": funcs_fail_messages,
    }
)
new_ds = new_ds.filter(lambda x: x["num_tests"] >= args.min_tests)
new_ds = new_ds.filter(lambda x: len(x["pass"]) > 0 and len(x["fail"]) > 0)
print(len(new_ds))
new_ds.push_to_hub(args.name, private=True)
