from llm_basics.train.train_1gpu import train
from datasets import load_dataset

data_dict = load_dataset("nuprl/MultiPL-T", split="racket").train_test_split(0.01)

train(
    model="arjunguha/notstarcoder-1b",
    global_batch_size=8,
    per_device_batch_size=1,
    epochs=7,
    learning_rate=3e-05,
    warmup_steps=100,
    train_data=data_dict["train"],
    eval_data=data_dict["test"],
    step_counter="exact",
    max_seq_length=2048,
    skip_last_batch=True,
    use_flash_attention_2=True
)