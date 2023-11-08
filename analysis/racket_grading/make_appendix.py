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
model_tuned = ("\\system{} Model", [(humaneval_id_to_name[i], programs[i]["tuned"]) for i in range(161) if programs[i]["tuned"] != ""])
data = [model_base, model_tuned]


def remove_comments(code):
    return "(define" + code.split("(define")[-1].strip()


def generate_latex_code(data):
    latex_code = ""
    
    base = data[0]
    tuned = data[1]
    
    latex_code = """\lstdefinestyle{qual_style}{
  language=Lisp,
  tabsize=2,
  showspaces=false,
  showstringspaces=false
}\n\n"""

    for i in range(len(base[1])):
        
        problem, racket_code = base[1][i]
        racket_code = remove_comments(racket_code)
        heading = base[0] + " - " + " ".join(problem.split( )[:2])
        latex_code += f"\\textbf{{{heading}}}\n\n"
        latex_code += "\\vspace{3mm}\n\n"
        line = f"\n\\begin{{lstlisting}}[basicstyle=\\ttfamily\\small,style=qual_style]\n{racket_code}\n\\end{{lstlisting}}\n\n"
        latex_code += line + "\\vspace{10mm}\n\n"
        
        problem, racket_code = tuned[1][i]
        racket_code = remove_comments(racket_code)
        heading = tuned[0] + " - " + " ".join(problem.split( )[:2])
        latex_code += f"\\textbf{{{heading}}}\n\n"
        latex_code += "\\vspace{3mm}\n\n"
        line = f"\n\\begin{{lstlisting}}[basicstyle=\\ttfamily\\small,style=qual_style]\n{racket_code}\n\\end{{lstlisting}}\n\n"
        latex_code += line
        
        latex_code += "\\pagebreak\n\n"
    
    return latex_code


table = generate_latex_code(data)
with open("appendix.tex", "w") as f:
    f.write(table)