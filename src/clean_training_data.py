import datasets 
import argparse
import re

def clean_racket(ex):
    content_lines = ex["content"].split("\n")
    # regex to recognize linestart ;; #
    canon_regex = re.compile(r"^\s*;;\s*#.*$")
    comment_line = re.compile(r"^\s*;;\s*$")
    whitespace = re.compile(r"^\s*$")
    new_content = []
    for line in content_lines:
        if not re.match(canon_regex, line) and re.match(comment_line, line) and re.match(whitespace, line): 
            new_content.append(line)
    ex["content"] = "\n".join(new_content)
    return ex

def clean_lua(ex):
    sol = ex["content"]
    sol = sol[:sol.find("\nlu =")]
    sol_lines = sol.split("\n")
    # TODO: the "**" is the old format, remove this later
    if "** Canonical Python Solution **" in sol_lines[0] or "## Canonical Python Solution ##" in sol_lines[0]:
        # remove the canonical solution from comment
        # canonical solution lines start with " * "
        not_canonical_i = 0
        for i, line in enumerate(sol_lines):
            if not line.startswith("-- *") and not line.startswith("-- #"):
                not_canonical_i = i
                break
        sol_lines = sol_lines[not_canonical_i:]

    # remove every line that is empty
    sol_lines = [line for line in sol_lines if line.rstrip() !=
                 "--" and line != ""]

    sol = "\n".join(sol_lines)
    ex["content"] = sol
    return ex

    


    

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input-dataset", type=str, required=True, help="Input dataset name")
    parser.add_argument("--output-dataset", type=str, required=True, help="Output dataset name")
    parser.add_argument("--language", type=str, required=True, help="Language of the dataset")
    parser.add_argument("--push", action="store_true", help="Push to hub")
    args = parser.parse_args()
    
    dataset = datasets.load_dataset(args.input_dataset)
    if args.language == "racket":
        dataset = dataset.map(clean_racket)
    elif args.language == "lua":
        dataset = dataset.map(clean_lua)
    else:
        # crash with unimplemented language
        raise NotImplementedError(f"Language {args.language} not implemented")

    if args.push:
        dataset.push_to_hub(args.output_dataset)
    elif args.output_dataset.endswith(".jsonl"):
        dataset.to_json(args.output_dataset)
    else:
        dataset.save_to_disk(args.output_dataset)
