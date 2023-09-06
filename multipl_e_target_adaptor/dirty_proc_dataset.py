'''
This script takes the raw filtered python functions with generated tests 
and processes them into individual files in the HumanEval format. 
Cli provided in __main__.
'''
import datasets
import os

CANNONICAL_LINE = "### Canonical solution below ###"
UNIT_LINE = "### Unit tests below ###"


def write_row_to_file(prefix, content_col):
    if not os.path.exists(prefix):
        os.mkdir(prefix)

    def writer(row):
        entrypoint = row["entrypoint"]
        file = f"{prefix}/HumanEval_{row['id']}_{entrypoint}.py"
        fixed_tests = "\n\t".join(
            [t.replace(entrypoint, "candidate") for t in row["tests"]])
        content = row[content_col]
        if content is None:
            content = row["content"]
        start_asserts = content.find("\nassert ")
        if start_asserts != -1:
            content = content[:start_asserts]
        fixed_content = proc_content(content)
        test_body = f"def check(candidate):\n\t{fixed_tests}\ndef test_check():\n\tcheck({entrypoint})\n"
        with open(file, "w") as f:
            f.write(fixed_content)
            f.write("\n\n")
            f.write(UNIT_LINE + "\n")
            f.write(test_body)
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

    return before + (" " * 4) + CANNONICAL_LINE + "\n" + body


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset", type=str,
                        default="nuprl/stack-dedup-python-testgen-starcoder-filter-v2",
                        help="Dataset of functions with tests to process")

    parser.add_argument("--output", type=str, default="stack-clean-python", help="Output directory")
    parser.add_argument("--min_coverage", type=float,
                        default=90, help="Minimum coverage of tests")
    parser.add_argument("--content_col", type=str,
                        default="content", help="Column name of content")
    args = parser.parse_args()
    ds = datasets.load_dataset(args.dataset, split="train")
    dir_of_this_script = os.path.dirname(os.path.realpath(__file__))
    ds = ds.filter(lambda x: x["coverage"] >= args.min_coverage)
    path = f"{dir_of_this_script}/{args.output}"
    print(f"Writing {len(ds)} rows to {path}")
    ds.map(write_row_to_file(path, args.content_col),
           load_from_cache_file=False)
