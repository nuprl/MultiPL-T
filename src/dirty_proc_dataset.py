import datasets
import os

CANNONICAL_LINE = "### Canonical solution below ###"
UNIT_LINE = "### Unit tests below ###"


def write_row_to_file(prefix):
    if not os.path.exists(prefix):
        os.mkdir(prefix)

    def writer(row):
        entrypoint = row["entrypoint"]
        file = f"{prefix}/HumanEval_{row['id']}_{entrypoint}.py"
        fixed_tests = "\n\t".join(
            [t.replace(entrypoint, "candidate") for t in row["tests"]])
        fixed_content = proc_content(row['content'])
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
    ds = datasets.load_dataset(
        "nuprl/stack-dedup-python-testgen-starcoder-filter", split="train")
    dir_of_this_script = os.path.dirname(os.path.realpath(__file__))
    up = os.path.dirname(dir_of_this_script)
    ds.map(write_row_to_file(f"{up}/stack-clean-python"),
           load_from_cache_file=False)
