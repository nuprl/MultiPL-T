import argparse 
import csv
from pathlib import Path

def get_items(name):
    # returns the number of training items
    split_name = name.split("_")
    return split_name[1]

if __name__ == "__main__": 
    cli = argparse.ArgumentParser()
    cli.add_argument("--exp-root", type=str, default="experiments")
    cli.add_argument("--out-dir", type=str, default="all_results.csv")
    args = cli.parse_args()

    exp_root = Path(args.exp_root)
    out_dir = Path(args.out_dir)
    out_dir.mkdir(exist_ok=True, parents=True)
    for d in exp_root.iterdir():
        if d.is_dir():
            exp_name = d.name
            training_items = get_items(exp_name)
            output = []
            for f in d.glob("*.csv"):
                with open(f, "r") as fp:
                    for (i, row) in enumerate(fp):
                        if i == 1:
                            output.append(row.strip())
            with open(out_dir / f"rkt_{training_items}", "w") as fp:
                writer = csv.writer(fp)
                writer.writerow(["Dataset", "Pass@k", "Estimate", "NumProblems", "MinCompletions", "MaxCompletions"])
                for row in output:
                    writer.writerow(row.split(","))
    


