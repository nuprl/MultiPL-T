from dataset_builder.generic_translator import translate_prompt_and_tests
from dataset_builder.humaneval_to_ml import Translator as MLTranslator
from dataset_builder.humaneval_to_lua import Translator as LuaTranslator
from dataset_builder.humaneval_to_rkt import Translator as RacketTranslator
import datasets


ds = datasets.load_dataset("nuprl/python-testgen-dirty-inferred", split="train")

def get_entrypoint(code):
    return code.split("def")[1].split("(")[0].strip()

entrypoint = get_entrypoint(EXAMPLE)

t = translate_prompt_and_tests(EXAMPLE, entrypoint, MLTranslator())
print(t)
