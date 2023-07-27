import sys
sys.path.append("..")
from code_llms.training import simple_train
from datasets import *
from pathlib import Path
import json 

TRAIN_DATA = [{ "content": "def factorial(n):\n    if n == 0:\n        return 1\n    else:\n        return n * factorial(n-2)\n"}]
TRAIN_DATA = Dataset.from_dict(TRAIN_DATA[0])

TEST_DATA = Path("10_humaneval/10_humaneval_lua.jsonl")
MODEL = "bigcode/starcoderbase-1b"

simple_train(MODEL,
            batch_size= 1,
            epochs=1,
            learning_rate= 1e5,
            warmup_steps= 1,
            train_data = TRAIN_DATA,
            test_data = TEST_DATA,
            dataset_limit= 50,
            log_dir= ".")
