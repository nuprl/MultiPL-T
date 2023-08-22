from dataset_builder.generic_translator import translate_prompt_and_tests
from dataset_builder.humaneval_to_ml import Translator as MLTranslator
from dataset_builder.humaneval_to_lua import Translator as LuaTranslator
from dataset_builder.humaneval_to_rkt import Translator as RacketTranslator
import datasets
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type=str,
                    default="nuprl/python-testgen-dirty-inferred")
parser.add_argument("--name", type=str, required=True)
parser.add_argument("--limit", type=int, default=1000)
args = parser.parse_args()


ds = datasets.load_dataset(
    "nuprl/python-testgen-dirty-inferred", split="train")


def get_entrypoint(code):
    return code.split("def")[1].split("(")[0].strip()


new_ds = {
    "content": [],
    "name": [],
}

ml_translator = MLTranslator()
lua_translator = LuaTranslator()
racket_translator = RacketTranslator()
translators = [ml_translator, lua_translator, racket_translator]

tries = 0
for ex in ds:
    tries += 1
    if len(new_ds["content"]) >= args.limit:
        break

    code = ex["content"]
    entrypoint = get_entrypoint(code)

    bad = False
    for translator in translators:
        r = translate_prompt_and_tests(code, entrypoint, translator)
        if r is None:
            bad = True
            break

    if bad:
        continue

    new_ds["content"].append(code)
    new_ds["name"].append(ex["name"])

print(len(new_ds["content"]))
print(tries)

new_ds = datasets.Dataset.from_dict(new_ds)
new_ds.push_to_hub(args.name, private=True)
