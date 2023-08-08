import datasets
import argparse


parser = argparse.ArgumentParser()
parser.add_argument('--ds1', type=str, required=True)
parser.add_argument('--lang1', type=str, required=True)
parser.add_argument('--ds2', type=str, required=True)
parser.add_argument('--lang2', type=str, required=True)
parser.add_argument('--len', type=int, required=True)
parser.add_argument('--name', type=str, required=True)
args = parser.parse_args()
# 50/50 split of the datasets
ds1 = datasets.load_dataset(args.ds1, split='train')
ds2 = datasets.load_dataset(args.ds2, split='train')

split_len = args.len // 2

assert len(ds1) >= split_len, f'Dataset 1 is too small: len(ds1)={len(ds1)}'
assert len(ds2) >= split_len, f'Dataset 2 is too small: len(ds2)={len(ds2)}'

ds1 = ds1.shuffle()
ds2 = ds2.shuffle()


interleaved = {
    "content": [],
    "id": [],
    "lang": [],
}
for i in range(split_len):
    interleaved["content"].append(ds1[i]["content"])
    interleaved["id"].append(ds1[i]["id"])
    interleaved["lang"].append(args.lang1)
    interleaved["content"].append(ds2[i]["content"])
    interleaved["id"].append(ds2[i]["id"])
    interleaved["lang"].append(args.lang2)

interleaved = datasets.Dataset.from_dict(interleaved)
interleaved.push_to_hub(args.name)
