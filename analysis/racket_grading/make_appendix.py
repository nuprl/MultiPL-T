from glob import glob
import re

programs = [{"base": "", "tuned":""} for _ in range(161)]
humaneval_id_to_name = {}


def extract_number(label):
    number = re.search(r'\d+', label)
    if number:
        return int(number.group())
    return 0

for model in ["tuned", "base"]:
    for file in glob(f"deanon_renamed_grading_dir/{model}/*.rkt"):
        humaneval_id = extract_number(file.split("HumanEval")[-1])
        humaneval_id_to_name[humaneval_id] = "HumanEval" + file.split("HumanEval")[-1].replace(".rkt", "").replace("_", " ")
        content = open(file).read().replace("#lang racket\n", "").strip()
        programs[humaneval_id][model] = content

# make data where data = [(problem, base_model, tuned_model)]
data = []
for i in range(161):
    if programs[i]["base"] != "" and programs[i]["tuned"] != "":
        data.append((humaneval_id_to_name[i], programs[i]["base"], programs[i]["tuned"]))
        
model_base = ("Base Model", [(humaneval_id_to_name[i], programs[i]["base"]) for i in range(161) if programs[i]["base"] != ""])
model_tuned = ("Tuned Model", [(humaneval_id_to_name[i], programs[i]["tuned"]) for i in range(161) if programs[i]["tuned"] != ""])
data = [model_base, model_tuned]


def generate_latex_code(data):
    latex_code = ""
    
    for model_name, problem_code_pairs in data:
        latex_code += f"\\textbf{{{model_name}}}\n"
        for problem, racket_code in problem_code_pairs:
            latex_code += f"\n\\begin{{lstlisting}}[language=Lisp,caption={problem},captionpos=t,basicstyle=\\ttfamily]\n{racket_code}\n\\end{{lstlisting}}\n\n\n"
    
    return latex_code


table = generate_latex_code(data[:1])
with open("appendix.tex", "w") as f:
    f.write(table)