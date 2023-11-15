import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

data = {
    "Racket MultiPl-T": [0, 0, 0, 50, 50, 0, 5, 0, 34, 50, 0, 50, 50, 0],
    "Racket Stack": [0, 0, 5, 50, 0, 0, 0, 41, 0, 3, 0, 50, 50, 0],
    "Lua MultiPl-T": [12, 0, 0, 50, 50, 0, 0, 50, 0, 50, 41, 50, 50, 6],
    "Lua Stack": [50, 0, 0, 0, 46, 0, 0, 50, 0, 50, 45, 50, 50, 0],
    "Ocaml MultiPl-T": [50, 0, 0, 50, 50, 0, 0, 50, 0, 50, 0, 0, 50, 0],
    "Ocaml Stack": [50, 0, 0, 50, 5, 0, 0, 50, 50, 49, 50, 0, 50, 0],
}

problems = [
    "Add Left of NumTree",
    "Add Subway Station",
    "Android Only",
    "Electrify Band Instruments",
    "Collatz Depth",
    "Decode Message",
    "Verify Source",
    "Mirror a Tree",
    "Double Do It",
    "Points and Lines",
    "Series",
    "Shapes",
    "Social Time",
    "Start AES",
]

df = pd.DataFrame(data)
# add problem col
df["Problem"] = problems

# data_by_problem 
df_by_problem = df.set_index("Problem").T
df = df_by_problem

colors = {'Lua': 'green', 'Ocaml': 'red', 'Racket': 'blue'}
fig, ax = plt.subplots(figsize=(20, 8))

idx_to_model = {i:e.split()[-1] for i,e in enumerate(list(df.index))}
idx_to_lang = {i:e.split()[0] for i,e in enumerate(list(df.index))}
# print(idx_to_model)

bar_width = 0.3
def hatch(lang):
    return 'xxx' if 'MultiPl-T' in lang else ''
collect_offsets = []
offset = 0
space=0.5
for i, col in enumerate(df.columns):
    non_zero_indices = np.where(df[col] != 0)[0]
    x_bars = [e+(bar_width*k) for k,e in enumerate([offset]*len(non_zero_indices))]
    ax.bar(
        x_bars,
        list(df[col][non_zero_indices]),
        width=bar_width,
        color=[colors[idx_to_lang[j]] for j in non_zero_indices],
        hatch=[hatch(idx_to_model[j]) for j in non_zero_indices],
        label=col,
    )

    collect_offsets.append(offset)
    if x_bars == []:
        offset += space
    else:
        offset = max(x_bars) + space


print(collect_offsets, len(collect_offsets))
ax.set_xticks(collect_offsets)
ax.set_xticklabels(problems, rotation=45, ha='right') 

import matplotlib.patches as mpatches
patches = [mpatches.Patch(color='red', label='OCaml Stack'),
    mpatches.Patch(facecolor='red', label='OCaml MultiPL-T', hatch='xxx', linewidth=0.7),
    mpatches.Patch(color='green', label='Lua Stack'),
    mpatches.Patch(facecolor='green', label='Lua MultiPL-T', hatch='xxx', linewidth=0.7),
    mpatches.Patch(color='blue', label='Racket Stack'),
    mpatches.Patch(facecolor='blue', label='Racket MultiPL-T', hatch='xxx', linewidth=0.7)]
plt.legend(handles=patches)


plt.tight_layout()
plt.show()
plt.savefig('adv.pdf')