from code_llms.training import simple_train
from pathlib import Path
import datasets

train_data = datasets.load_dataset("json", data_files="../../datasets/train_lua.jsonl", split="train")

# The name of the directory in which this file is located. E.g., if the file is
# called "/home/arjun/experiment1/train.py", then THIS_DIRECTORY_NAME is "experiment1".
THIS_DIRECTORY_NAME = Path(".").absolute().name

simple_train(
    model="/work/arjunguha-research-group/arjun/models/starcoderbase-1b",
    batch_size=8,
    epochs=7,
    learning_rate=3e-5,
    warmup_steps=10,
    train_data=train_data,
    test_data="../../MultiPL-E/prompts/humaneval-lua-reworded.jsonl",
    dataset_limit=None,
    schedule="cosine",
    log_dir="log",
)