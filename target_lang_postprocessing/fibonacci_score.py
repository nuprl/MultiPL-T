import datasets
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('--dataset', type=str, required=True)
parser.add_argument('--col', type=str, default='content')
args = parser.parse_args()


ds = datasets.load_dataset(args.dataset, split='train')

fib_count = 0
fibonacci_count = 0
ds_len = len(ds)
for i, row in enumerate(ds):
    if 'fibonacci' in row[args.col]:
        fibonacci_count += 1
    elif 'fib' in row[args.col]:
        fib_count += 1


print(f'fib: {fib_count}')
print(f'fibonacci: {fibonacci_count}')
print(f'fibonacci score: {(fib_count + fibonacci_count) / ds_len}')
