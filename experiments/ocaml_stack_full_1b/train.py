from code_llms.training import simple_train
from datasets import load_dataset, Dataset
from functools import partial

def gen_from_iterable_dataset(iterable_ds):
    yield from iterable_ds
    
# yields approx num_steps as ocaml_full_1b (3051 steps x epoch on 1 gpu)
split_size = 3000
scheme_data = load_dataset("bigcode/starcoderdata", 
                           split="train", 
                           data_dir=f"ocaml", 
                           streaming=True)
scheme_data = scheme_data.take(split_size)
TRAIN_DATA = Dataset.from_generator(partial(gen_from_iterable_dataset, scheme_data))

# save for reproducibility
TRAIN_DATA.save_to_disk("./ocaml_train")

TEST_DATA = "../../datasets/humaneval-ml.jsonl"
MODEL = "bigcode/starcoderbase-1b"

# lr=3e-5, batch_size=8, warmup_steps=10, epochs=7
simple_train(MODEL,
            batch_size= 8,
            epochs=7,
            learning_rate= 3e-5,
            warmup_steps= 10,
            train_data = TRAIN_DATA,
            test_data = TEST_DATA,
            dataset_limit= None,
            log_dir= "log",
            schedule="cosine")