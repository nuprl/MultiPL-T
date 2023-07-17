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
        fixed_tests = "\n\t".join([t.replace(entrypoint, "candidate") for t in row["tests"]])
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
    comment_block = []
    header = ""
    for line in lines:
        if line.startswith("#"):
            comment_block.append(line.replace("#", ""))
        elif line.startswith("def"):
            header = line 
            break
    comment_text = "\n    ".join(comment_block)
    docstring = f'    """{comment_text}"""'
    body = "\n".join(lines[len(comment_block)+1:])
    return f"{header}\n{docstring}\n\t{CANNONICAL_LINE}{body}".encode('utf-8', 'ignore').decode('utf-8')

    
if __name__ == "__main__":
    ds = datasets.load_dataset("nuprl/stack-dedup-python-testgen-starcoder-format", split="train")
    ds.map(write_row_to_file("/home/jgouwar/Git/research/multipl-t/stack-clean-python"), load_from_cache_file=False)
