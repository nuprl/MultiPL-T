import argparse 
import csv
from pathlib import Path

def parse_params(name):
    parsed = name.split("_")
    lr = float(parsed[1])
    bs = int(parsed[3])
    sched = parsed[5]
    return lr, bs, sched

if __name__ == "__main__": 
    cli = argparse.ArgumentParser()
    cli.add_argument("--exp-root", type=str, default="experiments")
    cli.add_argument("--outfile", type=str, default="all_results.csv")
    args = cli.parse_args()

    exp_root = Path(args.exp_root)
    outfile = Path(args.outfile)
    output = []
    for d in exp_root.iterdir():
        if d.is_dir():
            pak = -1.0 
            with open(d / "passk_checkpoint_final.csv", "r") as f:
                reader = csv.DictReader(f)
                for row in reader:
                    pak = float(row["Estimate"])
            output.append((d.name, pak))
    output.sort(key=(lambda p: p[1]), reverse=True)
    with open(outfile, "w") as f:
        for (name, pak) in output:
            lr, bs, sched = parse_params(name)
            f.write(f"{lr},{bs},{sched},{pak}\n") 
    
    
