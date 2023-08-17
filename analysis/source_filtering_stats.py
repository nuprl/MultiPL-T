import matplotlib.pyplot as plt

all_functions = 22_311_478
with_docstrings = 5_359_051
with_typecheck = 459_280
no_bench = 432_361

# losses
no_docstrings = all_functions - with_docstrings
no_typecheck = with_docstrings - with_typecheck
todos_benchmarks_non_english = with_typecheck - no_bench

# Data for bar chart
steps = [
    "All functions",
    "With docstrings",
    "Typechecked and returns",
    "No TODOs/Benchmarks/English",
]
counts = [all_functions, with_docstrings, with_typecheck, no_bench]

# Calculate the losses at each step
losses = [0, no_docstrings, no_typecheck, todos_benchmarks_non_english]


def get_text_position(yval, loss_val, max_val):
    if yval < max_val * 0.1:
        return yval + (0.15 * max_val)
    elif yval < max_val * 0.3:
        return yval + (0.1 * max_val)
    elif yval - loss_val < max_val * 0.1:  # If there's a significant drop due to loss
        return yval + (0.05 * max_val)
    else:
        return yval + (0.02 * max_val)


plt.rcParams.update({'font.size': 15})
# Plotting the bar chart with losses
fig, ax = plt.subplots(figsize=(10, 6))
bars = ax.bar(steps, counts, color=['blue', 'green', 'orange', 'purple'])
y_limit = 1.10 * max(counts)

# Annotate values on the bars and plot the losses
for i, bar in enumerate(bars):
    yval = bar.get_height()
    position = get_text_position(yval, losses[i], y_limit)
    ax.text(bar.get_x() + bar.get_width()/2, position,
            int(yval), ha='center', va='center')

    # Plot the loss line if it's not the first bar
    if i != 0:
        loss_start = yval + losses[i]
        ax.plot([bar.get_x(), bar.get_x() + bar.get_width()],
                [loss_start, loss_start], color='red', linewidth=2)
        ax.text(bar.get_x() + bar.get_width()/2, loss_start + (0.02 * y_limit),
                f"-{losses[i]}", ha='center', va='center', color='red')

ax.set_ylim(0, y_limit)
ax.set_title("Bar chart showing number of functions after each filtering step")
ax.set_ylabel("Number of functions")
plt.xticks(rotation=15)
plt.tight_layout()
plt.grid(True, which="both", ls="--", c='0.7')
plt.savefig("./figures/source_filtering_stats.png")
plt.savefig("./figures/source_filtering_stats.pdf")
