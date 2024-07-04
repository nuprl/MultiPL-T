#### 0. Configure environment

```
pip3 -m venv venv
source venv/bin/activate
pip3 install -r requirements.txt
```

# Target Language Processing Directory

This directory contains all the code to process the generations in the target language for then using them in the training of the model.

The main script of interest is `multipl_e_results_to_hf.py`, which takes in the completions from MultiPL-E, processes them, deduplicates them, and uploads them to Hugging Face.

The script can be utilized as follows:

```
python3 multipl_e_results_to_hf.py --path <path to the completions> --name <output repo name on huggingface> --lang <target language>
```

### Deduplication

The script utilizes the parallelized ROUGE-L-based deduplication algorithm presented in our paper. By default, it will deduplicate completions
only in buckets based on their prompt. To deduplicate completions across all prompts, use the `--global_dedup` flag.

Other arguments can be tweaked to change the behavior of the deduplication algorithm. `--global_dedup_prob` controls the
probability of a pair of examples not being deduplicated despite being duplicates, across all prompts. Higher values will take longer to run but will
result in more deduplication hits. `--dedup_threshold` controls the threshold for the ROUGE-L score for two completions to be considered duplicates.

If deduplication is not desired, there are three options that can be utilized under the `--strategy` flag:

1. `random`: Randomly picks a single completion from each prompt bucket.
2. `all`: Just takes all completions without any deduplication.
3. `best` (experimental!): Utilizes a separate model trained to score completions based on quality and picks the best completion from each prompt bucket.
