import json
import os
import sys
import argparse
import gzip

def get_completions(file):
    with gzip.open(file, 'rt') as f:
        for line in f.readlines():
            yield json.loads(line)
            
def to_dir(file, outdir, lang):
    os.makedirs(outdir, exist_ok=True)
    for obj in get_completions(file):
        if obj["language"] != lang:
            continue
        # cast obj to keys ['name', 'results': ['status', 'program']]
        statuses = obj["status"]
        programs = [obj["prompt"] + p for p in obj["completion"]]
        obj = {"name": f"HumanEval_{obj['problem']}", 
               "results": [{"status": i[0], "program": i[1]} for i in zip(statuses, programs)]}
        # dump into a results.json.gz file
        with gzip.open(os.path.join(outdir, f"{obj['name']}.results.json.gz"), 'wt') as f:
            json.dump(obj, f, indent=2)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('file', type=str, help='Path to a .jsonl.gz file.')
    parser.add_argument('outdir', type=str)
    parser.add_argument('lang', type=str, help='Language of the completions.')
    args = parser.parse_args()
    to_dir(args.file, args.outdir, args.lang)