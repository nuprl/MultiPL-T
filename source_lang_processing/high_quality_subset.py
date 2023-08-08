import datasets
import subprocess
import tempfile
import hashlib
import time
import os
from has_return import does_have_return
from typing import List, Tuple, Dict


# runs pyright in the given directory, returns stdout
# then, it logs the number of errors for each file
def run_pyright(d):
    try:
        outs = subprocess.run(
            ["pyright", "*"],
            cwd=d,
            capture_output=True,
            timeout=120,
            text=True,
        ).stdout
    except Exception as e:
        print(e)
        return None

    cur_file = ""
    filemap = {}
    lines = outs.split("\n")
    for i, line in enumerate(lines):
        if i == len(lines) - 2:
            break

        if line.startswith("  "):
            if "- error:" in line:
                filemap[cur_file] += 1
        else:
            file = line.split("/")[-1]
            filemap[file] = 0
            cur_file = file

    return filemap


def typecheck_batch(files: List[str]) -> Dict[str, str]:
    # Create a temporary directory using the tempfile module
    filemap: Dict[str, str] = {}
    with tempfile.TemporaryDirectory() as tempdir:
        for contents in files:
            hash_object = hashlib.sha1(bytes(contents, "utf8"))
            hex_dig = hash_object.hexdigest()
            filemap[hex_dig] = contents
            name = os.path.join(tempdir, hex_dig + ".py")
            with open(name, "w") as f:
                f.write(contents)

        # Run pyright in the temporary directory
        typecheck_map = run_pyright(tempdir)
        if typecheck_map is None:
            return {}

    for contents, errors in typecheck_map.items():
        no_py = contents.replace(".py", "")
        if errors == 0:
            continue

        if no_py in filemap:
            del filemap[no_py]

    return filemap


ds = datasets.load_dataset("nuprl/stack-dedup-python-fns",
                           data_dir="data", split="train")

BATCH_SIZE = 1000
batch = []
max_i = len(ds) - 1

new_ds = {
    "content": [],
    "sha1": [],
    "id": [],
}

e_id = 0


def estimate_time_to_finish(start_time, end_time, i, max_i):
    elapsed_time = end_time - start_time
    remaining_items = max_i - i
    remaining_batches = remaining_items / \
        BATCH_SIZE if remaining_items % BATCH_SIZE == 0 else remaining_items // BATCH_SIZE + 1
    est_time = elapsed_time * remaining_batches
    return est_time


for i, ex in enumerate(ds):
    code = ex["content"]

    if not does_have_return(code):
        continue
    batch.append(code)

    if len(batch) == BATCH_SIZE or i == max_i:
        start_time = time.time()
        filemap = typecheck_batch(batch)
        for sha1, contents in filemap.items():
            new_ds["content"].append(contents)
            new_ds["sha1"].append(sha1)
            new_ds["id"].append(e_id)
            e_id += 1

        end_time = time.time()
        print(
            f"[{i}] Finished batch (num typecheck: {len(filemap)}) {i} of {max_i} ({i/max_i})")
        print(f"[{i}] Took: {end_time - start_time} (est to finish: {estimate_time_to_finish(start_time, end_time, i, max_i)})")
        batch = []



new_ds_hf = datasets.Dataset.from_dict(new_ds)
new_ds_hf.push_to_hub("nuprl/stack-dedup-python-fns-returns-typechecks")
