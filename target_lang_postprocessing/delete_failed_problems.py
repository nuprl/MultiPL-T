from progressbar import progressbar
from pathlib import Path
from argparse import ArgumentParser
import gzip
import json

parser = ArgumentParser()
parser.add_argument("path", help="Path to the directory containing the results files")
args = parser.parse_args()
def make_path_iterator(): return Path(args.path).glob("**/*.results.json.gz")

for path in progressbar(make_path_iterator(), max_value=len(list(make_path_iterator()))):
    with gzip.open(path, "rt") as f:
        data = json.load(f)

    results = data["results"]
    func_tests = data["tests"]

    sols = []
    num_failed = 0
    num_passed = 0
    for res in results:
        if res["exit_code"] == 0:
            num_passed += 1
        else:
            num_failed += 1

    if num_passed == 0:
        print(f"Deleting {path} with {num_failed} failed problems")
        filename = path.name.split(".")[0] + ".json.gz"
        comp_path = path.parent / filename
        path.unlink()
        print(f"Deleting {comp_path}")
        comp_path.unlink()


