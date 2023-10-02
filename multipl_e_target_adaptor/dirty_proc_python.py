'''
This script produces a MultiPL-E jsonl prompt of the python testgen dataset, without running the
translator.

Example element:
{
  "name": "HumanEval_0_has_close_elements",
  "language": "py",
  "prompt": "from typing import List\n\ndef has_close_elements(numbers: List[float], threshold: float) -> bool:\n    \"\"\" Check if in given list of numbers, are any two numbers closer to each other than\ngiven threshold.\n>>> has_close_elements([1.0, 2.0, 3.0], 0.5)\nFalse\n>>> has_close_elements([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 0.3)\nTrue\"\"\"\n",
  "doctests": "keep",
  "original": "/home/arjun/repos/nuprl/MultiPL-E/datasets/../datasets/originals/HumanEval_0_has_close_elements.py",
  "prompt_terminology": "verbatim",
  "tests": "def check(candidate):\n    assert candidate([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3) == True\n    assert candidate([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05) == False\n    assert candidate([1.0, 2.0, 5.9, 4.0, 5.0], 0.95) == True\n    assert candidate([1.0, 2.0, 5.9, 4.0, 5.0], 0.8) == False\n    assert candidate([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1) == True\n    assert candidate([1.1, 2.2, 3.1, 4.1, 5.1], 1.0) == True\n    assert candidate([1.1, 2.2, 3.1, 4.1, 5.1], 0.5) == False\n\ndef test_check():\n    check(has_close_elements)\n\ntest_check()\n",
  "stop_tokens": [
    "\ndef",
    "\n#",
    "\nif",
    "\nclass"
  ]
}
'''
import datasets
import os
import json


def write_row_to_file(file, content_col, no_union):
    def writer(row):
        entrypoint = row["entrypoint"]
        fixed_tests = "\n\t".join(
            [t.replace(entrypoint, "candidate") for t in row["tests"]])
        content = row[content_col]
        if content is None or (no_union and "Union" in content):
            content = row["content"]
        start_asserts = content.find("\nassert ")
        if start_asserts != -1:
            content = content[:start_asserts]
        fixed_content = proc_content(content)
        test_body = f"def check(candidate):\n\t{fixed_tests}\ndef test_check():\n\tcheck({entrypoint})\ntest_check()\n"

        obj = {
            "name": f"{row['id']}_{entrypoint}",
            "language": "py",
            "prompt": fixed_content,
            "doctests": "keep",
            "original": "None",
            "prompt_terminology": "verbatim",
            "tests": test_body,
            "stop_tokens": ["\ndef", "\n#", "\nif", "\nclass"]
        }

        with open(file, "a") as f:
            f.write(json.dumps(obj) + "\n")

        return row
    return writer


def proc_content(content):
    lines = content.split("\n")
    before = ""
    body = ""
    num_docstring = 0
    for line in lines:
        if num_docstring == 2:
            body += line + "\n"
        else:
            before += line + "\n"
        if '"""' in line and num_docstring < 2:
            num_docstring += line.count('"""')

    return before


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset", type=str,
                        default="nuprl/stack-dedup-python-testgen-starcoder-filter-v2",
                        help="Dataset of functions with tests to process")

    parser.add_argument("--output", type=str,
                        default="py-prompts.jsonl", help="Output file")
    parser.add_argument("--min_coverage", type=float,
                        default=90, help="Minimum coverage of tests")
    parser.add_argument("--content_col", type=str,
                        default="content", help="Column name of content")
    parser.add_argument("--no-union", action="store_true",
                        help="Rejects programs that have union types, samples the untyped program instead")
    args = parser.parse_args()
    ds = datasets.load_dataset(args.dataset, split="train")
    dir_of_this_script = os.path.dirname(os.path.realpath(__file__))
    ds = ds.filter(lambda x: x["coverage"] >= args.min_coverage)
    path = f"{dir_of_this_script}/{args.output}"
    print(f"Writing {len(ds)} rows to {path}")
    # create the file
    with open(path, "w") as f:
        f.write("")

    ds.map(write_row_to_file(path, args.content_col, args.no_union),
           load_from_cache_file=False)
