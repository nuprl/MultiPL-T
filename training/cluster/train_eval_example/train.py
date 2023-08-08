from code_llms.training import simple_train
from pathlib import Path
import datasets

train_data = None # load train data here 
test_data = None # load test data here

# The name of the directory in which this file is located. E.g., if the file is
# called "/home/arjun/experiment1/train.py", then THIS_DIRECTORY_NAME is "experiment1".
THIS_DIRECTORY_NAME = Path(".").absolute().name

# Validation
if train_data is None:
    raise ValueError("Please load train data into the variable 'train_data'.")
if test_data is None:
    raise ValueError("Please load test data into the variable 'test_data'.")


simple_train(
    model="/work/arjunguha-research-group/arjun/models/starcoderbase-1b",
    batch_size=8,
    epochs=5,
    learning_rate=1e-5,
    warmup_steps=10,
    train_data=train_data,
    test_data=test_data,
    dataset_limit=None,
)
