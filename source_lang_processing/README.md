#### 0. Configure environment

```
pip3 -m venv venv
source venv/bin/activate
pip3 install -r requirements.txt
```

# Source Language Processing Directory

This directory contains all scripts for processing the source Python dataset that will be then used
for semi-synthetic data generation.

## Extract Python Functions With Docstrings from The Stack

To extract raw Python functions with docstrings from the Stack, run the following command:

```
python3 generate_from_the_stack --num-workers <cpus needed> --output <output repo name on huggingface>
```

A pre-extracted dataset is available at `nuprl/stack-dedup-python-fns` on Hugging Face.

## Filter The Dataset

We then filter the dataset to remove functions that do not typecheck or that do not return a value.
You can run the following command to filter the dataset:

```
python3 high_quality_subset.py --dataset <dataset name on huggingface> --push <output repo name on huggingface>
```

For this step too, a pre-filtered dataset is available at `nuprl/stack-dedup-python-fns-returns-typechecks` on Hugging Face.

## Source Language Test Generation Instructions (Python)

#### 1. Run the code execution docker container

```
pushd ./code_exec_server/
./build_and_run.sh
popd
```

#### 2. Run the dataset aggregator server

This will start a server that will listen for incoming requests to add to the dataset. Open a new terminal and run:

```
python3 dataset_aggregator.py --name <output repo name on huggingface>
```

#### 3. Run the test generator script

```
accelerate config # and follow the instructions
accelerate launch test_gen.py
```

## Further Filtering and Transformation -- Remove Benchmark Data and Type Inference

After test generation, further filtering is needed to remove benchmark data and infer python types
using test cases. To do this, run the following command:

```
python3 filter_and_transform.py --dataset <dataset name on huggingface> --result <output repo name on huggingface> --inference
```

**WARNING**: this kind of type inference utilizes code execution, which could potentially be dangerous. use with caution.

We provide two pre-processed datasets, one with type inference and one without:

- Plain: `nuprl/stack-dedup-python-testgen-starcoder-filter-v2`
- Type-inferred: `nuprl/stack-dedup-python-testgen-starcoder-filter-inferred-v2`
