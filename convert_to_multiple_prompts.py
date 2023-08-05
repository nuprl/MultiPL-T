from argparse import ArgumentParser
from pathlib import Path
import json

SOLUTION_TOKEN = "<solution>\n"
TEST_TOKEN = "<tests>\n"

lang_to_stop_tokens = {
    "rkt": ["\n(define ", "\n#|", "\n;","\n("],
    "ocaml": ["\n\n", "\n(*", "\ntype"],
    "lua":  ["\nlocal", "\nfunction", "\n--", "\n\n"]
}

def get_prompt(code):
    prompt = code[:code.index(SOLUTION_TOKEN)]
    return prompt.rsplit("\n", 1)[0]

def get_test(code):
    return code[code.index(TEST_TOKEN) + len(TEST_TOKEN):]

def main(directory, language, extension, save):
    multiple_prompts = [] # MultiPL-E expects
    for p in directory.glob(f"*.{extension}"):
        with open(p) as f:
            code = f.read()
        new_prompt = {
            "name": p.stem,
            "language": language,
            "prompt": get_prompt(code),
            "doctests": "original",
            "original": str(p),
            "prompt_terminology": "verbatim",
            "tests": get_test(code),
            "stop_tokens": lang_to_stop_tokens[language]
        }
        multiple_prompts.append(new_prompt)
    with open(save, "wt+") as f:
        json.dump(multiple_prompts, f, indent=2)

if __name__ == "__main__":
    args = ArgumentParser(description="Extracts a directory of examples into humaneval prompts")
    args.add_argument("directory", help="Location of the directory holding the new problems", type=Path)
    args.add_argument("language", help="The MultiPL-E language identifier", type=str)
    args.add_argument("extension", help="The extension of the files of the new problems.")
    args.add_argument("save", help="The location to save the prompts to", type=Path)
    args = args.parse_args()
    main(args.directory, args.language, args.extension, args.save)