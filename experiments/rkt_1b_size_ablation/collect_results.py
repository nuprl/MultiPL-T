from pathlib import Path
DIRECTORIES = [
    "5k_results", 
    "10k_results", 
    "15k_results", 
    "20k_results", 
    "25k_results",
    "30k_results",
    "35k_results",
    "40k_results"
]

def parse_filename(fname: Path): 
    split = fname.name.split(".")[0].split("_")
    if split[2] == "final":
        return 0 
    else:
        return int(split[2])

def get_results(directory: Path):
    results = []
    for f in directory.iterdir():
        if f.is_file():
            step_num = parse_filename(f)
            with open(f, "r") as f:
                for (i, line) in enumerate(f):
                    if i == 1:
                        results.append([step_num, line])
    results.sort(key=lambda x: x[0])
    # add the first step to the last step to get the final step count 
    results[0][0] = results[1][0] + results[len(results) - 1][0]
    results.sort(key=lambda x: x[0])
    return results 

def write_results(outfile, results): 
    with open(outfile, "w") as out:
        for row in results:
            line = ",".join(map(str, row))
            out.write(line)

if __name__ == "__main__":
    for dir in DIRECTORIES: 
        outfile = dir + "_all_results.csv"
        path = Path(dir)
        write_results(outfile, get_results(path))
    