# /// script
# requires-python = "==3.12"
# dependencies = [
#     "torch",
#     "transformers",
#     "datasets",
#     "accelerate",
#     "scipy",
#     "tensorboard"
# ]
# [tool.uv]
# exclude-newer = "2025-08-08T00:00:00Z"
# ///
from llm_basics.train.train_1gpu import train
from datasets import load_dataset
import torch
import random
import numpy as np

random.seed(0)
np.random.seed(0)
torch.manual_seed(0)

data_dict = load_dataset("nuprl/MultiPL-T", split="racket").shuffle(seed=42)
test_data = data_dict.select(range(0, 200))
train_data = data_dict.select(range(200, len(data_dict)))

train(
    model="arjunguha/notstarcoder-1b",
    global_batch_size=8,
    per_device_batch_size=2,
    epochs=1,
    learning_rate=3e-05,
    warmup_steps=100,
    train_data=train_data,
    eval_data=test_data,
    step_counter="exact",
    max_seq_length=2048,
    skip_last_batch=True,
    use_flash_attention_2=True
)
