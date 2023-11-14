import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import numpy as np

def read_with_path(csvpath):
    df = pd.read_csv(csvpath)
    df['path'] = csvpath
    return df

def parse_lang_from_path(path):
    split = path.split("/")
    config = split[-2]
    return config.split("_")[0]

def parse_dataset_name(ds_name):
    return int(ds_name.split("_")[1])

def remap_language(lang):
    mapping = {
        'ocaml': 'OCaml', 
        'rkt': 'Racket', 
        'lua': 'Lua',
        'r': 'R',
        'jl': 'Julia'
    }
    return mapping.get(lang, "Invalid language")

data_files = [
    "../../experiments/ocaml_subset_1b/results.csv",
    "../../experiments/rkt_subset_1b/results.csv",
    "../../experiments/lua_subset_1b/results.csv",
    #"../../experiments/r_subset_1b/results.csv",
    #"../../experiments/jl_subset_1b/results.csv"
]

raw_ds_list = [read_with_path(f) for f in data_files]
subset_ds = pd.concat(raw_ds_list)
subset_ds['language'] = subset_ds['path'].apply(lambda path: remap_language(parse_lang_from_path(path)))
subset_ds['step_num'] = subset_ds['Dataset'].apply(parse_dataset_name)
subset_ds['passk'] = subset_ds['Estimate']
subset_ds['num_tokens'] = 2048 * subset_ds['step_num']
subset_ds = subset_ds[['language', 'passk', 'num_tokens']]

# Subset the data

# Add baseline data
base_rkt = pd.DataFrame({'language': ['Racket'], 'passk': [0.047], 'num_tokens': [0]})
base_ocaml = pd.DataFrame({'language': ['OCaml'], 'passk': [0.015], 'num_tokens': [0]})
base_lua = pd.DataFrame({'language': ['Lua'], 'passk': [0.121], 'num_tokens': [0]})
base_julia = pd.DataFrame({'language': ['Julia'], 'passk': [0.113], 'num_tokens': [0]}) 
base_r = pd.DataFrame({'language': ['R'], 'passk': [0.054], 'num_tokens': [0]})
subset_ds = pd.concat([subset_ds, base_rkt, base_ocaml, base_lua])

# Plotting
plt.figure(figsize=(10, 6))
plt.legend(loc='upper left', bbox_to_anchor=(1, 0.6))
plt.xlabel("Number of training tokens")
plt.ylabel("Pass@1")

plt.xlim(-1000000, 3e7)
plt.ylim(0, 0.16)
plt.xticks([0, 1e7, 2e7, 3e7])
plt.yticks(np.arange(0.0, 0.18, 0.01))
plt.gca().xaxis.set_major_formatter(lambda x, _: f'{x:,.0e}')
for lang, df in subset_ds.groupby("language"):
    points = sorted(zip(df["num_tokens"], df["passk"]), key=lambda x: x[0])
    plt.plot([p[0] for p in points], [p[1] for p in points], label=lang, marker='o')
    plt.legend()
plt.legend(title="Language")
plt.savefig("subset.pdf", bbox_inches='tight')