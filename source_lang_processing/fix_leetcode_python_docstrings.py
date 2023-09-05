"""
This script exists because the person who uploaded the original leetcode dataset didn't check that 
it was actually correct. The original python solutions dataset has completely different docstrings
than the expected implementation. I already ran a testgen run that took a while on that dataset, 
so I didn't want to throw away the resources. This script attempts to fix the docstrings by
finding matching function signatures in our leetcode javascript dataset (which we checked is correct!!!!)
and transplanting the docstrings from there.

See the discussion here: https://huggingface.co/datasets/mhhmm/leetcode-solutions-python/discussions/1#64b8ca2c77ae61bcc802e7f9
"""

import datasets
import hashlib

lc_js = datasets.load_dataset("nuprl/leetcode-js", split="train")


name_to_ex = {}


def get_fn_sig_from_js(code):
    for line in code.split("\n"):
        # var twoSum = function(nums, target) {
        if line.startswith("var "):
            name = line.split(" ")[1]
            # args
            args = line.split("(")[1].split(")")[0].split(",")
            args = [arg.strip() for arg in args]
            args = [arg for arg in args if arg]
            str_args = ", ".join(args)
            return f"{name}({str_args})"


def get_fn_sig_from_py(code):
    for line in code.split("\n"):
        # def twoSum(nums, target):
        # or
        # def twoSum(nums: List[int], target: int) -> List[int]:
        # remove type annotations if present
        if line.startswith("def "):
            name = line.split(" ")[1].split("(")[0]
            # args
            args = line.split("(")[1].split(")")[0].split(",")
            # remove typeo
            args = [arg.split(":")[0] for arg in args]
            args = [arg.strip() for arg in args]
            args = [arg for arg in args if arg]
            str_args = ", ".join(args)
            return f"{name}({str_args})"


def convert_snake_to_camel_case(name):
    # preserves camel case if present
    if "_" not in name:
        return name
    title = [w.capitalize() for w in name.split("_")]
    camel = title[0].lower() + "".join(title[1:])
    return camel


dups = set()

for ex in lc_js:
    sig = get_fn_sig_from_js(ex["Code"])
    if sig in name_to_ex:
        dups.add(sig)
        del name_to_ex[sig]
    elif sig not in dups:
        name_to_ex[sig] = ex


print(f"Dups removed: ", len(dups))
print(f"Total unique: ", len(name_to_ex))

lc_testgen = datasets.load_dataset(
    "nuprl/leetcode-solutions-python-testgen-gpt4", split="train"
)

new_ds = {
    "topics": [],
}

del_cols = ["problem_name_worded", "full_code", "signature"]

for name in lc_testgen.column_names:
    new_ds[name] = []

for del_col in del_cols:
    del new_ds[del_col]

not_found = 0
for ex in lc_testgen:
    content = ex["content"]
    sig = get_fn_sig_from_py(content)
    sig = convert_snake_to_camel_case(sig)
    if sig not in name_to_ex:
        not_found += 1
        continue
    orig_ex = name_to_ex[sig]
    orig_doc = orig_ex["Body"]
    orig_topics = orig_ex["Topics"]
    ex["topics"] = orig_topics

    # make orig_doc into a docstring (indented by 4 spaces)
    orig_doc = "\n".join(["    " + line for line in orig_doc.split("\n")])
    orig_doc = '    """\n' + orig_doc + '"""'

    # transplant the docstring. remove the old one, add the new one
    lines = content.split("\n")
    buf = ""
    buf_no_code = ""
    in_doc = False
    had_doc = False
    for line in lines:
        if line.lstrip().startswith('"""') and not had_doc:
            in_doc = not in_doc
            if in_doc:
                buf += orig_doc + "\n"
            else:
                had_doc = True
                buf_no_code = buf
        elif not in_doc:
            buf += line + "\n"

    print(buf_no_code)
    ex["content"] = buf
    ex["prompt"] = buf_no_code
    ex["problem_name_coded"] = orig_ex["titleSlug"]
    ex["difficulty"] = orig_ex["Difficulty"]
    ex["sha1"] = hashlib.sha1(buf.encode("utf-8")).hexdigest()

    for key, val in ex.items():
        if key not in new_ds:
            continue
        new_ds[key].append(val)


print(f"Total: ", len(lc_testgen))
print(f"Not found: ", not_found)
print(f"Remaining: ", len(new_ds["content"]))

print(new_ds["content"][123])
new_ds = datasets.Dataset.from_dict(new_ds)
print(new_ds.column_names)
print(new_ds.to_pandas().head())
new_ds.push_to_hub("nuprl/leetcode-solutions-python-testgen-fix", private=True)
