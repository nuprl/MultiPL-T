from code_llms.training import simple_train
from pathlib import Path
import datasets

train_data = datasets.load_dataset("bigcode/starcoderdata", data_dir="scheme", split="train")

# The name of the directory in which this file is located. E.g., if the file is
# called "/home/arjun/experiment1/train.py", then THIS_DIRECTORY_NAME is "experiment1".
THIS_DIRECTORY_NAME = Path(".").absolute().name

simple_train(
    model="bigcode/starcoderbase-1b",
    batch_size=8,
    epochs=3,
    learning_rate=3e-5,
    warmup_steps=10,
    train_data=train_data,
    test_data="../../MultiPL-E/prompts/humaneval-rkt-reworded.jsonl",
    dataset_limit=None,
    schedule="cosine",
    log_dir="log",
)
