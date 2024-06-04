import datasets
import numpy as np
ds = datasets.load_dataset(
    "nuprl/stack-dedup-python-testgen-starcoder-filter-v2", split="train")

# len lines

len_lines = []
for ex in ds:
    len_lines.append(len(ex["content"].split("\n")))

print(np.mean(len_lines))
print(np.std(len_lines))

# branches
branches = []
for ex in ds:
    code = ex["content"]
    branches.append(code.count("if") + code.count("elif") + code.count("else") + code.count("while") +
                    code.count("for") + code.count("try") + code.count("except") + code.count("finally"))

print(np.mean(branches))
print(np.std(branches))
