import argparse
import pandas as pd
import json
import matplotlib.pyplot as plt
import numpy as np
import re


def compare_grades(file1, file2):
    """
    For each model and problem, give mean score between graders and stdev.
    For each model and grader, calculate overall mean and standard deviation.
    Plot each problem and grader score for each model.
    """
    # mean score between graders, +- error, and stdev
    grader1 = pd.read_csv(file1)
    grader2 = pd.read_csv(file2)
    compare_graders = pd.merge(grader1, grader2, on=["problem"])
    
    # add a column that gets the per problem mean for base_model_score_x and base_model_score_y
    compare_graders["mean_base"] = compare_graders[["base_model_score_x", "base_model_score_y"]].mean(axis=1)
    compare_graders["abs_diff_base"] = abs(compare_graders["base_model_score_x"] - compare_graders["base_model_score_y"])
    # compare_graders["stdev_base"] = compare_graders[["base_model_score_x", "base_model_score_y"]].std(axis=1)
    # compare_graders["coeff_var_base"] = compare_graders["stdev_base"] / compare_graders["mean_base"]
    compare_graders["mean_tuned"] = compare_graders[["tuned_model_score_x", "tuned_model_score_y"]].mean(axis=1)
    compare_graders["abs_diff_tuned"] = abs(compare_graders["tuned_model_score_x"] - compare_graders["tuned_model_score_y"])
    # compare_graders["stdev_tuned"] = compare_graders[["tuned_model_score_x", "tuned_model_score_y"]].std(axis=1)
    # compare_graders["coeff_var_tuned"] = compare_graders["stdev_tuned"] / compare_graders["mean_tuned"]

    diff_graders = compare_graders[(compare_graders["abs_diff_base"] > 0) | (compare_graders["abs_diff_tuned"] > 0)]
    
    diff_graders.to_csv("disagree_graders.csv", index=False)
    
    compare_graders.to_csv("compare_graders.csv", index=False)
    
    # problems where base mean is significantly >= 2 worse than tuned mean
    base_worse = compare_graders[compare_graders["mean_base"] - compare_graders["mean_tuned"] > 0]
    base_worse.to_csv("tuned_worse.csv", index=False)
    # problems where tuned mean is significantly >= 2 worse than base mean
    tuned_worse = compare_graders[compare_graders["mean_tuned"] - compare_graders["mean_base"] > 0]
    tuned_worse.to_csv("base_worse.csv", index=False)
    # problems where both tuned and base mean are bad < 10
    n = 13
    both_bad = compare_graders[(compare_graders["mean_tuned"] < n) & (compare_graders["mean_base"] < n)]
    both_bad.to_csv("both_bad.csv", index=False)
    
    # plot mean for each problem
    plot_mean_problem(compare_graders[compare_graders["problem"] != "total"])
    
    overall_compare(grader1, grader2)
    
    # plot each problem and grader score for each model
    grader1, grader2 = grader1[grader1["problem"] != "total"], grader2[grader2["problem"] != "total"]
    plot_for_model("base_model_score", grader1, grader2, bar_colors=('g', 'y'))
    plot_for_model("tuned_model_score", grader1, grader2, bar_colors=('blue', '#C8A2C8'))
    
    plot_grouped_bar(grader1,grader2)
    

def overall_compare(grader1, grader2):
    # overall mean and stdev for each grader and model
    overall = {}
    for i,g in enumerate([grader1, grader2]):
        # exlude row "total"
        total_tuned = int(g[g["problem"] == "total"]["tuned_model_score"].values[0])
        total_base = int(g[g["problem"] == "total"]["base_model_score"].values[0])
        g = g[g["problem"] != "total"]
        mean_base = g["base_model_score"].mean()
        stdev_base = g["base_model_score"].std()
        mean_tuned = g["tuned_model_score"].mean()
        stdev_tuned = g["tuned_model_score"].std()
        overall[f"grader_{i+1}"] = {"mean_base": mean_base, 
                                    "stdev_base": stdev_base, 
                                    "mean_tuned": mean_tuned, 
                                    "stdev_tuned": stdev_tuned,
                                    "total_base": total_base,
                                    "total_tuned": total_tuned,
                                    "base_total_%": total_base / (35*15),
                                    "tuned_total_%": total_tuned / (35*15)}
        
    overall["mean"] = {"mean_base": (overall["grader_1"]["mean_base"] + overall["grader_2"]["mean_base"]) / 2,
                       "mean_tuned": (overall["grader_1"]["mean_tuned"] + overall["grader_2"]["mean_tuned"]) / 2,
                        "total_base": (overall["grader_1"]["total_base"] + overall["grader_2"]["total_base"]) / 2,
                        "total_tuned": (overall["grader_1"]["total_tuned"] + overall["grader_2"]["total_tuned"]) / 2,
                        "base_total_%": (overall["grader_1"]["base_total_%"] + overall["grader_2"]["base_total_%"]) / 2,
                        "tuned_total_%": (overall["grader_1"]["tuned_total_%"] + overall["grader_2"]["tuned_total_%"]) / 2}
    with open("overall_graders.json", "w") as f:
        json.dump(overall, f, indent=4)
    
def extract_number(label):
    number = re.search(r'\d+', label)
    if number:
        return int(number.group())
    return 0

        
def plot_mean_problem(compare_graders):
    compare_graders = compare_graders.iloc[compare_graders['problem'].map(extract_number).argsort()]
    # plot mean_base against mean_tuned for each problem
    fig, ax = plt.subplots(figsize=(10, 3))  # Creating a single figure and axis
    # bar plot
    total_bars = len(compare_graders)  # Total number of bars
    bar_width = 0.35  # Width of each bar
    index = np.arange(total_bars)  # Index for the bars
    index0 = index - bar_width
    index1 = index
    ax.bar(index0, compare_graders["mean_base"], width=bar_width, label='Base Model', alpha=0.6, color='r')
    ax.bar(index1, compare_graders["mean_tuned"], width=bar_width, label='MultiPL-T Model', alpha=0.6, color='b')
    # ax.set_title("Mean Score for Each Problem")
    ax.set_xlabel("HumanEval Problem Id")
    ax.set_ylabel("Mean Style Score")
    ax.set_xticks(index)  # Set x-ticks at the index positions
    ax.set_xticklabels(compare_graders["problem"].apply(extract_number).tolist())
    ax.set_xlim(-1, total_bars)
    ax.set_yticks(np.arange(16))  # Set y-axis ticks from 0 to 15
    plt.legend(loc='lower left')  # Set legend location
    plt.tight_layout() 
    plt.savefig("mean_qual_eval_scores.pdf")


def plot_for_model(model, grader1, grader2, title="", bar_colors=('b', 'r')):
    # Sort the data by the numeric part extracted from the labels
    grader1 = grader1.iloc[grader1['problem'].map(extract_number).argsort()]
    grader2 = grader2.iloc[grader2['problem'].map(extract_number).argsort()]

    fig, ax = plt.subplots(figsize=(10, 3))  # Creating a single figure and axis

    total_bars = len(grader1)  # Total number of bars
    bar_width = 0.35  # Width of each bar

    grader1_pos = np.arange(total_bars)  # Positions for Grader 1
    grader2_pos = grader1_pos + bar_width  # Positions for Grader 2, shifted by bar_width

    # Plot for Grader 1 for the specified model
    ax.bar(grader1_pos, grader1[model], width=bar_width, label='Grader 1', color=bar_colors[0], alpha=0.6)

    # Plot for Grader 2 for the specified model
    ax.bar(grader2_pos, grader2[model], width=bar_width, label='Grader 2', color=bar_colors[1], alpha=0.6)

    # ax.set_title(title)
    ax.set_xlabel("HumanEval Problem")
    ax.set_ylabel("Racket Style Score")
    ax.set_xticks(grader1_pos + bar_width / 2)

    # Modify x-axis labels to truncate to just include the number
    ax.set_xticklabels([extract_number(label) for label in grader1["problem"]], rotation=45)

    ax.set_yticks(np.arange(16))

    plt.legend(loc='lower left') 
    # plt.subplots_adjust(left=0.1, right=0.5, bottom=0.1, top=0.9)

    plt.tight_layout()

    plt.savefig(f"comparison_{model}.pdf")
    plt.show()  # Optional, to display the plot



def plot_grouped_bar(grader1, grader2):
    grader1 = grader1.iloc[grader1['problem'].map(extract_number).argsort()]
    grader2 = grader2.iloc[grader2['problem'].map(extract_number).argsort()]
    
    labels = grader1["problem"].apply(extract_number).tolist()
    
    fig, ax = plt.subplots(figsize=(12, 6))  # Creating a single figure and axis

    total_bars = len(grader1)  # Total number of bars
    bar_width = 0.2  # Width of each bar
    index = np.arange(total_bars)  # Index for the bars

    # Plotting the grouped bars
    index0 = index - bar_width
    index1 = index
    index2 = index + bar_width
    index3 = index + 2 * bar_width
    ax.bar(index0, grader1["base_model_score"], width=bar_width, label='Base Model - Grader 1', alpha=0.6, color='y')
    ax.bar(index1, grader1["tuned_model_score"], width=bar_width, label='MultiPL-T Model - Grader 1', alpha=0.6, color='g')
    ax.bar(index2, grader2["base_model_score"], width=bar_width, label='Base Model - Grader 2', alpha=0.6, color='#C8A2C8')
    ax.bar(index3, grader2["tuned_model_score"], width=bar_width, label='MultiPL-T Model - Grader 2', alpha=0.6, color='blue')

    # ax.set_title("Comparison of Scores")
    ax.set_xlabel("HumanEval Problem Id")
    ax.set_ylabel("Style Score")
    ax.set_xticks(index)  # Set x-ticks at the index positions
    ax.set_xticklabels(labels)
    ax.set_xlim(-1, total_bars)
    
    ax.set_yticks(np.arange(16))  # Set y-axis ticks from 0 to 15

    plt.legend(loc='lower left')  # Set legend location

    plt.tight_layout()

    plt.savefig("full_qual_eval_scores.pdf")
    plt.show()  # Optional, to display the plot

def deanonmize(number, secret_key="../secret_keys/secret_key_hash.json"):
    with open(secret_key, "r") as f:
        secret_key = json.load(f)
        if number in secret_key["base"]:
            return "base"
        else:
            return "tuned"

def grading_item_analysis(scoresheet1, scoresheet2):
    """
    find discrepancies between graders' grading items
    qualify categories where base/tuned lost most points
    """
    items = scoresheet1.columns[2:]
    merged = pd.merge(scoresheet1, scoresheet2, on=["File"])

    # find any grading items where the graders disagree and put in a list: problem, item, grader1, grader2
    disagree = []
    for i in range(len(merged)):
        row = merged.iloc[i]
        for item in items:
            if row[f"{item}_x"] != row[f"{item}_y"]:
                disagree.append([row["File"], item, row[f"{item}_x"], row[f"{item}_y"]])
    disagree = pd.DataFrame(disagree, columns=["problem", "item", "grader1", "grader2"])
    disagree.to_csv("disagree_items.csv", index=False)
    
    # deanonimize merged by mapping onto file 
    merged["File"] = merged["File"].apply(lambda x: x + "_" + deanonmize(extract_number(x)))
    
    # for each grading item, see how often it was deducted for base
    base_deducted = {item: {"ded":0,"count":0} for item in items}
    tuned_deducted = {item: {"ded":0,"count":0} for item in items}
    for i in range(len(merged)):
        row = merged.iloc[i]
        for item in items:
            if row[f"{item}_x"] > 0 or row[f"{item}_y"] < 0:
                if "base" in row["File"]:
                    base_deducted[item]["ded"] += ((row[f"{item}_x"] + row[f"{item}_y"])/2)
                    base_deducted[item]["count"] += 1
                else:
                    tuned_deducted[item]["ded"] += ((row[f"{item}_x"] + row[f"{item}_y"])/2)
                    tuned_deducted[item]["count"] += 1
    
    with open("deductions.json", "w") as f:
        json.dump({"base": base_deducted, "tuned": tuned_deducted}, f, indent=4)

                
    
    
    

def main(args):
    if args.do_scoresheet:
        scoresheet1 = pd.read_csv(args.file1)
        scoresheet2 = pd.read_csv(args.file2)
        grading_item_analysis(scoresheet1, scoresheet2)
    elif args.do_analysis:
        compare_grades(args.file1, args.file2)
    elif args.do_pair:
        pass

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Compare two grading stats files")
    parser.add_argument("file1", help="First grading stats file")
    parser.add_argument("file2", help="Second grading stats file")
    parser.add_argument("-a", "--do_analysis", help="Stats analysis", action="store_true")
    parser.add_argument("-p", "--do_pair", help="Pair grading", action="store_true")
    parser.add_argument("-s", "--do_scoresheet", help="scoresheet", action="store_true")
    args = parser.parse_args()
    main(args)