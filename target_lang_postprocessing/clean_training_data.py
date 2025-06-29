'''
This script cleans the training data for the Lua, Racket, and OCaml datasets.
It removes lines that are blank, comments, or canonical solutions.
Cli provided in __main__.
'''
import datasets
import argparse
import re


def clean_racket(sol):
    content_lines = sol.split("\n")
    # find "(require rackunit)" line
    # remove everything after that
    for i, line in enumerate(content_lines):
        if "(require rackunit)" in line:
            content_lines = content_lines[:i]
            break

    # regex to recognize linestart ;; #
    canon_regex = re.compile(r"^\s*;;\s*#.*$")
    comment_line = re.compile(r"^\s*;;\s*$")
    whitespace = re.compile(r"^\s*$")
    new_content = []
    for line in content_lines:
        if not (re.match(canon_regex, line) or re.match(comment_line, line) or re.match(whitespace, line) or "** Canonical Python Solution **" in line):
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
        for i, line in enumerate(sol_lines[1:]):
            r_i = i + 1
            if not line.startswith("-- *") and not line.startswith("-- #"):
                not_canonical_i = r_i
                break
        sol_lines = sol_lines[not_canonical_i:]

    # remove every line that is empty
    sol_lines = [line for line in sol_lines if line.rstrip() !=
                 "--" and line != ""]

    return "\n".join(sol_lines)


def clean_luau(sol):
    sol = sol[:sol.find("\nlocal function _half_equals")]
    sol_lines = sol.split("\n")
    if "## Canonical Python Solution ##" in sol_lines[0]:
        # remove the canonical solution from comment
        # canonical solution lines start with " * "
        not_canonical_i = 0
        for i, line in enumerate(sol_lines[1:]):
            r_i = i + 1
            if not line.startswith("-- *") and not line.startswith("-- #"):
                not_canonical_i = r_i
                break
        sol_lines = sol_lines[not_canonical_i:]

    # remove every line that is empty
    sol_lines = [line for line in sol_lines if line.rstrip() !=
                 "--" and line != ""]

    return "\n".join(sol_lines)


def clean_py(sol):
    sol = sol[:sol.find("\ndef check(candidate)")]
    sol_lines = sol.split("\n")
    sol_lines = [line for line in sol_lines if line.rstrip() != ""]
    return sol


def clean_ml(sol):
    sol = sol[:sol.find("\nlet assertions")]
    sol_lines = sol.split("\n")
    if "## Canonical Python Solution ##" in sol_lines[0]:
        not_canonical_i = 0
        for i, line in enumerate(sol_lines[1:]):
            r_i = i + 1
            if not line.strip().startswith("#"):
                not_canonical_i = r_i
                break
        sol_lines = sol_lines[not_canonical_i:]
        sol_lines = ["(**"] + sol_lines
    sol_lines = [line for line in sol_lines if line.rstrip() != ""]
    return "\n".join(sol_lines)


def clean_julia(sol):
    sol = sol[:sol.find("\nusing Test")]
    sol_lines = sol.split("\n")
    if "## Canonical Python Solution ##" in sol_lines[0]:
        not_canonical_i = None
        for i, line in enumerate(sol_lines[1:]):
            r_i = i + 1
            if not line.strip().startswith("#"):
                not_canonical_i = r_i
                break
        assert not_canonical_i
        sol_lines = sol_lines[not_canonical_i:]
        sol_lines = ['"""'] + sol_lines
    sol_lines = [line for line in sol_lines if line.rstrip() != ""]
    return "\n".join(sol_lines)


def clean_r(sol):
    sol = sol[:sol.find("\ntest_humaneval")]
    sol_lines = sol.split("\n")
    if "## Canonical Python Solution ##" in sol_lines[0]:
        not_canonical_i = None
        for i, line in enumerate(sol_lines[1:]):
            r_i = i + 1
            if line.count("#") < 2:
                not_canonical_i = r_i + 1
                break

        assert not_canonical_i
        sol_lines = sol_lines[not_canonical_i:]
    sol_lines = [line for line in sol_lines if line.rstrip() != ""]
    return "\n".join(sol_lines)   # since end is the stop token


def clean_ex(ex, cleaner):
    ex["content"] = cleaner(ex["content"])
    return ex


def clean_luau_ex(ex): return clean_ex(ex, clean_luau)
def clean_lua_ex(ex): return clean_ex(ex, clean_lua)
def clean_racket_ex(ex): return clean_ex(ex, clean_racket)
def clean_ml_ex(ex): return clean_ex(ex, clean_ml)
def clean_julia_ex(ex): return clean_ex(ex, clean_julia)
def clean_r_ex(ex): return clean_ex(ex, clean_r)


# if __name__ == "__main__":
#     parser = argparse.ArgumentParser()
#     parser.add_argument("--input-dataset", type=str,
#                         required=True, help="Input dataset name")
#     parser.add_argument("--output-dataset", type=str,
#                         required=True, help="Output dataset name")
#     parser.add_argument("--language", type=str, required=True,
#                         help="Language of the dataset")
#     parser.add_argument("--push", action="store_true", help="Push to hub")
#     args = parser.parse_args()

#     if args.input_dataset.endswith(".jsonl"):
#         dataset = datasets.load_dataset(
#             "json", data_files=args.input_dataset, split="train")
#     else:
#         dataset = datasets.load_dataset(args.input_dataset, split="train")
#     cleaner = None
#     if args.language == "racket":
#         cleaner = clean_racket_ex
#     elif args.language == "lua":
#         cleaner = clean_lua_ex
#     elif args.language == "ml":
#         cleaner = clean_ml_ex
#     elif args.language == "julia":
#         cleaner = clean_julia_ex
#     elif args.language == "r":
#         cleaner = clean_r_ex
#     else:
#         # crash with unimplemented language
#         raise NotImplementedError(f"Language {args.language} not implemented")

#     dataset = dataset.map(cleaner)

#     if args.push:
#         dataset.push_to_hub(args.output_dataset)
#     elif args.output_dataset.endswith(".jsonl"):
#         dataset.to_json(args.output_dataset)
#     else:
#         dataset.save_to_disk(args.output_dataset)




# --- clean_training_data.py 更新部分 ---
import re

# 言語ごとのコメント・テスト開始トークン設定
LANG_CONFIG = {
    # Lisp/Clojure
    'clj':    {'line_comment': ';',    'block_comment': ('#_', ''),       'stop_tokens': ['(deftest', '(ns']},
    # C family
    'cpp':    {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['TEST(', 'int main']},
    'cs':     {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['[Test]', 'static void Main']},
    # Dart
    'dart':   {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['test(', 'void main']},
    # D
    'd':      {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['unittest', 'void main']},
    # Elixir
    'elixir': {'line_comment': '#',    'block_comment': ('"""','"""'), 'stop_tokens': ['ExUnit']},
    # Go
    'go':     {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['func Test']},
    # Haskell
    'hs':     {'line_comment': '--',   'block_comment': ('{-','-}'),     'stop_tokens': ['main =']},
    # Java / Scala / Kotlin family
    'java':   {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['@Test', 'public static void main']},
    'jl':     {'line_comment': '#',    'block_comment': ('#=','#='),      'stop_tokens': ['@test']},
    'js':     {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['describe(', 'test(']},
    'ts':     {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['describe(', 'test(']},
    # Lua
    'lua':    {'line_comment': '--',   'block_comment': ('--[[', ']]'),   'stop_tokens': ['function']},
    'luau':   {'line_comment': '--',   'block_comment': ('--[[', ']]'),   'stop_tokens': ['function']},
    # MATLAB
    'matlab': {'line_comment': '%',    'block_comment': ('%{','%}'),     'stop_tokens': ['function']},
    # OCaml / ML
    'ml':     {'line_comment': '(*',   'block_comment': ('(*','*)'),     'stop_tokens': ['let assertions']},
    # PHP / Prolog
    'php':    {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['function test', '<?php']},
    'pl':     {'line_comment': '%',    'block_comment': ('/*','*/'),     'stop_tokens': [':-']},
    # Ruby
    'rb':     {'line_comment': '#',    'block_comment': ('=begin','=end'), 'stop_tokens': ['RSpec.describe', 'def test']},
    # Racket
    'rkt':    {'line_comment': ';',    'block_comment': ('#|','|#'),     'stop_tokens': ['(require rackunit)']},
    # R
    'r':      {'line_comment': '#',    'block_comment': (None, None),     'stop_tokens': ['test_humaneval']},
    # Rust
    'rs':     {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['fn main', 'assert!']},
    # Scala
    'scala':  {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['should', 'object']},
    # Shell
    'sh':     {'line_comment': '#',    'block_comment': (None, None),     'stop_tokens': ['exit']},
    # Swift
    'swift':  {'line_comment': '//',   'block_comment': ('/*','*/'),     'stop_tokens': ['XCTest', 'func test']},
    # F#
    'fs':     {'line_comment': '//',   'block_comment': ('(*','*)'),     'stop_tokens': ['[<Test>]']},
}


def clean_generic(sol: str, lang: str) -> str:
    """
    指定言語のコメント削除・テスト部切り取り・空行除去を行う汎用クリーナー
    """
    spec = LANG_CONFIG.get(lang)
    if not spec:
        raise ValueError(f"Unsupported language: {lang}")
    # テスト開始位置で切り取り
    for tok in spec['stop_tokens']:
        idx = sol.find(tok)
        if idx != -1:
            sol = sol[:idx]
            break
    # ブロックコメント削除
    start, end = spec['block_comment']
    if start and end:
        pattern = re.escape(start) + r'.*?' + re.escape(end)
        sol = re.sub(pattern, '', sol, flags=re.DOTALL)
    # 行コメント削除
    prefix = spec['line_comment']
    lines = sol.split('\n')
    lines = [l for l in lines if not prefix or not l.strip().startswith(prefix)]
    # 空行除去
    lines = [l for l in lines if l.strip()]
    return '\n'.join(lines)

# 各言語用ラッパー
for _lang in LANG_CONFIG.keys():
    globals()[f'clean_{_lang}'] = (lambda l: (lambda sol: clean_generic(sol, l)))(_lang)

