import csv
from pathlib import Path
FILES = [f for f in Path('.').iterdir() if f.is_file() and f.suffix == '.csv']

results = {}
for f in FILES:
    num_exs = f.name.split('_')[0]
    # read f as a csv file 
    with open(f, "r") as f:
        reader = csv.reader(f)
        data = list(reader)[1:]
        results[num_exs] = max(map(lambda r: (int(r[0]), float(r[2])), data), key=lambda x: x[1])
print(results)



