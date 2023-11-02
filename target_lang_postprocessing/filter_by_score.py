import code_scorer.inference
import datasets
import argparse
import random
from tqdm import tqdm


def chunkify(lst, n):
    chunks = []
    for i in range(0, len(lst), n):
        chunk = []
        for j in range(n):
            if i + j < len(lst):
                chunk.append(lst[i + j])
        chunks.append(chunk)
    return chunks


parser = argparse.ArgumentParser()
parser.add_argument('--dataset', type=str, required=True)
parser.add_argument('--split', type=str, default='train')
parser.add_argument('--score_filter', type=float, default=0.0)
parser.add_argument('--push', type=str, required=True)
args = parser.parse_args()

random.seed(args.seed)

ds = datasets.load_dataset(args.dataset, split=args.split)

scorer = code_scorer.inference.CodeScorer("nuprl/code-scorer-edu-v1")

dedup_sig = set()

scores = []
chunks = chunkify(ds, 128)
for exs in tqdm(chunks, total=len(chunks)):
    codes = [e['content'] for e in exs]
    c_scores = scorer.score(codes)
    scores.extend(c_scores)

ds = ds.add_column('score', scores)
ds = ds.filter(lambda x: x['score'] >= args.score_filter)
ds.push_to_hub(args.push, private=True)
