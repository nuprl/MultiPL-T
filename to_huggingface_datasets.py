import datasets
import json
from pathlib import Path
from typing import Generator
from argparse import ArgumentParser

pa = ArgumentParser()
pa.add_argument("--path", type=str, default="./multiplt_lua_20230712T1146.jsonl")
pa.add_argument("--name", type=str, default="nuprl/lua_self_instruct")
args = pa.parse_args()

train_data = datasets.load_dataset(
    "json", data_files=args.path, split="train")
print(train_data.to_pandas().head())


def map_content(item):
    content = item["content"][:item["content"].find("\nlu =")]
    matching = "\nlocal function"
    fn_name_p1 = content[content.find(matching)+len(matching):].lstrip()
    fn_name = fn_name_p1[:fn_name_p1.find("(")]
    return {"content": content, "entrypoint": fn_name}


train_data = train_data.map(map_content)

print(train_data.to_pandas().head())
print(train_data["content"][0])

train_data.push_to_hub(args.name)
