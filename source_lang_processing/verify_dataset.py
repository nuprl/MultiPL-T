# runs all the tests with the code in the dataset
import argparse
import requests
import datasets
from code_exec_server.code_exec_reqs import exec_test

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type=str)
parser.add_argument("--server", type=str, default="http://localhost:8000")
args = parser.parse_args()


ds = datasets.load_dataset(args.dataset, split="train")
ids = set()
for ex in ds:
    i = ex["id"]
    if i in ids:
        print(f"Duplicate id: {i}")
    ids.add(i)
    code = ex["content"]
    tests = ex["tests"]
    tests_str = "\n".join(tests)
    res = exec_test(args.server, code, tests_str)
    if not res:
        print(f"Failed to run tests for {i}")
