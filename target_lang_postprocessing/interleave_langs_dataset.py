import datasets
import json
from pathlib import Path
from typing import Generator


def read_jsonl(path: Path) -> Generator[dict, None, None]:
    with open(path) as f:
        for line in f:
            yield json.loads(line)


train_data = datasets.load_dataset(
    "json", data_files="./multiplt_lua_20230712T1146.jsonl", split="train")
print(train_data.to_pandas().head())

py_originals_map = {}
for file in Path("./stack-clean-python").glob("*.py"):
    strip_humaneval = file.stem[len("HumanEval_"):]
    name_after_number = strip_humaneval[strip_humaneval.find("_")+1:]
    text = file.read_text()
    original = text[:text.find("\n\n\n### Unit tests below ###")]
    # remove the ### Canonical solution below ### line
    original_lines = original.split("\n")
    original = ""
    for i, line in enumerate(original_lines):
        if not "### Canonical solution below ###" in line:
            original += line + "\n"
    py_originals_map[name_after_number] = original


def map_content(item):
    content = item["content"][:item["content"].find("\nlu =")]
    matching = "\nlocal function"
    fn_name_p1 = content[content.find(matching)+len(matching):].lstrip()
    fn_name = fn_name_p1[:fn_name_p1.find("(")]
    lua = content
    py = py_originals_map[fn_name]
    content = py + "\n" + lua
    print(content)
    return {"content": content, "entrypoint": fn_name, "lua": lua, "python": py}


train_data = train_data.map(map_content)

print(train_data.to_pandas().head())

train_data.push_to_hub("nuprl/multiplt_lua")
