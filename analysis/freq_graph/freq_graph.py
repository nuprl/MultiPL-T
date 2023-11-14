import matplotlib.pyplot as plt

BASE_DATA = {
    "C++": [0.06379,0.3056 ],
    "C#":[0.05823,0.2056 ],
    #["D",0,10.01 ],
    "Go": [0.03101,0.2147 ],
    "Java" :[0.11336,0.2853 ],
    "Javascript": [0.08437,0.3170 ],
    
    "PHP": [0.0939,0.2675 ],
    #"Perl": [0.00291,0.1632 ],
    "Python": [0.07875,0.3035 ],
    "Ruby": [0.00888,0.1725 ],
    "Rust": [0.01188,0.2446 ],
    #"Scala": [0.0,28.79 ],
    #["Bash",0,11.02 ],
    #["Swift",0,16.74 ],
    "Typescript": [0.03458,0.3215 ],
}

START_DATA = { 
    "Racket": [0.00004, 0.1177],
    "Lua": [0.00374, 0.2661],
    "OCaml": [0.00134, 0.069],
    "Julia": [0.00171, 0.2109],
    "R": [0.00039, 0.102],
}

TUNED_DATA = {
    "Racket": [0.00004, 0.210],
    "Lua": [0.00374, 0.310],
    "OCaml": [0.00134, 0.199],
    "Julia": [0.00171, 0.352],
    "R": [0.00039, 0.173],
}


if __name__ == "__main__":
    fig, ax = plt.subplots(figsize=(10, 6))
    for (label, data) in BASE_DATA.items():
        ax.scatter(data[0], data[1], label=label, color="black", alpha=0.3, s=50)
        ax.annotate(label, (data[0]+0.0015, data[1]))
    for label in START_DATA:
        start = START_DATA[label]
        tuned = TUNED_DATA[label]
        ax.scatter(start[0], start[1], color="black", alpha=0.5, s=50)
        ax.scatter(tuned[0], tuned[1], color="black", s=50)
        if label == "Racket":
            ax.annotate(label, (tuned[0]-0.0015, tuned[1]+0.005))
        else:
            ax.annotate(label, (tuned[0]+0.0015, tuned[1]))
        ax.annotate('', 
            xy=(tuned[0], tuned[1]), 
            xytext=(start[0], start[1]),
            arrowprops=dict(arrowstyle='->', color='grey', linestyle='dashed')
        )
        ax.set_xlabel("Fraction of Training Data")
        ax.set_ylabel("Pass@1 on MultiPL-E HumanEval")

    # save image as a pdf 
    fig.savefig("freq_graph.pdf", bbox_inches='tight')
