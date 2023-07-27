import datasets
import argparse
import re


def clean_racket(sol):
    content_lines = sol.split("\n")
    # regex to recognize linestart ;; #
    canon_regex = re.compile(r"^\s*;;\s*#.*$")
    comment_line = re.compile(r"^\s*;;\s*$")
    whitespace = re.compile(r"^\s*$")
    new_content = []
    for line in content_lines:
        if not re.match(canon_regex, line) and re.match(comment_line, line) and re.match(whitespace, line):
            new_content.append(line)
    return "\n".join(new_content)


def clean_lua(sol):
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

    return "\n".join(sol_lines)


def clean_ml(sol):
    return sol


def clean_ex(ex, cleaner):
    ex["content"] = cleaner(ex["content"])
    return ex


def clean_lua_ex(ex): return clean_ex(ex, clean_lua)
def clean_racket_ex(ex): return clean_ex(ex, clean_racket)
def clean_ml_ex(ex): return clean_ex(ex, clean_ml)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input-dataset", type=str,
                        required=True, help="Input dataset name")
    parser.add_argument("--output-dataset", type=str,
                        required=True, help="Output dataset name")
    parser.add_argument("--language", type=str, required=True,
                        help="Language of the dataset")
    parser.add_argument("--push", action="store_true", help="Push to hub")
    args = parser.parse_args()

    dataset = datasets.load_dataset(args.input_dataset)
    cleaner = None
    if args.language == "racket":
        cleaner = clean_racket_ex
    elif args.language == "lua":
        cleaner = clean_lua_ex
    elif args.language == "ml":
        cleaner = clean_ml_ex
    else:
        # crash with unimplemented language
        raise NotImplementedError(f"Language {args.language} not implemented")

    dataset = dataset.map(cleaner)

    if args.push:
        dataset.push_to_hub(args.output_dataset)
    elif args.output_dataset.endswith(".jsonl"):
        dataset.to_json(args.output_dataset)
    else:
        dataset.save_to_disk(args.output_dataset)
