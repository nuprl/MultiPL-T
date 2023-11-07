import matplotlib.pyplot as plt

BASE_DATA = {
    "C++": [6.379,30.56 ],
    "C#":[5.823,20.56 ],
    #["D",0,10.01 ],
    "Go": [3.101,21.47 ],
    "Java" :[11.336,28.53 ],
    "Javascript": [8.437,31.70 ],
    
    "PHP": [7.939,26.75 ],
    #"Perl": [0.291,16.32 ],
    "Python": [7.875,30.35 ],
    "Ruby": [0.888,17.25 ],
    "Rust": [1.188,24.46 ],
    "Scala": [0.0,28.79 ],
    #["Bash",0,11.02 ],
    #["Swift",0,16.74 ],
    "Typescript": [3.458,32.15 ],
}

START_DATA = { 
    "Racket": [0.004, 11.77],
    "Lua": [0.374, 26.61],
    "OCaml": [0.134, 6.9],
    "Julia": [0.171, 21.09],
    "R": [0.039, 10.2],
}

TUNED_DATA = {
    "Racket": [0.004, 21.0],
    "Lua": [0.374, 31.0],
    "OCaml": [0.134, 19.9],
    "Julia": [0.171, 35.2],
    "R": [0.039, 17.3],
}

def plot_data(data, ax: plt.Axes, color="black", marker='o', x_offset=0.0, y_offset=0.5, extra_annot=""):
    ax.scatter(
        [x[0] for x in data.values()], 
        [x[1] for x in data.values()], 
        marker=marker, 
        color=color
    )
    for p in data.items(): 
        ax.annotate(p[0]+extra_annot, (p[1][0] + x_offset, p[1][1] + y_offset))

if __name__ == "__main__":
    single_fig, sing_ax = plt.subplots(figsize=(10, 6)) 
    plot_data(BASE_DATA, sing_ax, color="black", marker='o')
    plot_data(START_DATA, sing_ax, color="green", marker='o')
    plot_data(TUNED_DATA, sing_ax, color="purple", marker='^', x_offset=-0.5, y_offset=-0.8, extra_annot="(T)")

    multi_fig, (max1, max2) = plt.subplots(1, 2, figsize=(10, 6), sharex=True, sharey=True)

    plot_data(BASE_DATA, max1, color="black", marker='o')
    plot_data(BASE_DATA, max2, color="black", marker='o')
    plot_data(START_DATA, max1, color="green", marker='^')
    plot_data(TUNED_DATA, max2, color="green", marker='^')
    max1.set_title("Before tuning with MultiPL-T")
    max2.set_title("After tuning with MultiPL-T")


    single_fig.savefig("single_freq_graph.png", bbox_inches='tight')
    multi_fig.savefig("multi_freq_graph.png", bbox_inches='tight')

