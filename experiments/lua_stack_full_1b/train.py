from code_llms.training import simple_train
from datasets import load_dataset, Dataset
from functools import partial

def gen_from_iterable_dataset(iterable_ds):
    yield from iterable_ds
    
# yields approx num_steps as lua_full_1b (3782 steps x epoch on 1 gpu)
split_size = 2800
scheme_data = load_dataset("bigcode/the-stack", 
                           split="train", 
                           data_dir=f"data/lua", 
                           streaming=True)
scheme_data = scheme_data.take(split_size)
TRAIN_DATA = Dataset.from_generator(partial(gen_from_iterable_dataset, scheme_data))

# save for reproducibility
TRAIN_DATA.save_to_disk("./lua_train")

open("./null_data.jsonl", "w").close()

TEST_DATA = "./null_data.jsonl"
MODEL = "bigcode/starcoderbase-1b"

# lr=3e-5, batch_size=8, warmup_steps=10, epochs=6
simple_train(MODEL,
            batch_size= 8,
            epochs=6,
            learning_rate= 3e-5,
            warmup_steps= 10,
            train_data = TRAIN_DATA,
            test_data = TEST_DATA,
            dataset_limit= None,
            log_dir= "log",
            schedule="cosine")
