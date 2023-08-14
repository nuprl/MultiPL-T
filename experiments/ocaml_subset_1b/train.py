# This script was run on Boa
from code_llms.training import simple_train
from pathlib import Path
import datasets

train_data = datasets.load_dataset("nuprl/multipl-t_ocaml_paper_aug", split="train")

simple_train(
    model="bigcode/starcoderbase-1b",
    batch_size=8,
    epochs=7,
    learning_rate=3e-5,
    warmup_steps=10,
    train_data=train_data,
    test_data="../../datasets/humaneval-ml.jsonl",
    dataset_limit=25000,
    log_dir="log",
)

