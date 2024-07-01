# Racket Grading

This directory contains code, data and results for our qualitative grading of Racket programs.

# Overview

## Code

- `compare_models.py`: Receives as input two paths containing the evaluated MultiPL-E results for a 
    tuned and a base model completions. It selects a subset of successful completions for grading: 

    ```
    select_for_grading/
            ├── base_counterparts/
            │   ├── {problem1_name}
            │   │    ├── program_1.rkt
            │   │    ├── program_2.rkt
            │   │    └── ...
            │   ├── {problem2_name}
            │        ├── program_1.rkt
            │        └── ...
            ├── tuned/
                ├── {problem1_name}
                │    ├── program_1.rkt
                │    ├── program_2.rkt
                │    └── ...
                ├── {problem2_name}
                     ├── program_1.rkt
                     └── ...
    ```
    We also further hand-select these examples due to near duplicates.

- `make_grading_data.py`: loads the selections from the `select_for_grading` directory and generates an anonimized
    grading dataset with pairs of example problem completions from the base model and tuned model. The order of pairs
    is randomized. A secret key is saved that keeps track of which completion came from which model.

- `stats.py`: takes the graders' graded scoresheets and a secret key to deanonimize completions, outputs the grading   
    statistics.

- `make_appendix.py`: formats results for the latex appendix.

- `utils.py` and `do_cat.sh` contain utility code.

## Data

- `grading_dir`: contains the anonimized examples provided to graders. `rename_grading_dir` has the same content
    as `grading_dir`, but displayes MultiPL-E problem names for readability. Similarly, `pair_grading_dir` contains
    paired problem completions for readability and ease of grading.

- `scoresheets`: contains the templates for scoresheets filled by graders.

- `secret_keys`: contains the mapping between anonimized completions and the model they came from.

## Results

- `grader_scores`: contains the results of grading and denonimization of the completions.

- `deanon_renamed_grading_dir`: contains the denonimized version of the grading examples.

- `grading_stats`: contains the grading statistics.

# Running

To replicate our grading pipeline, follow these steps:

1. Run `compare_models.py` to select examples for grading.
2. Run `make_grading_data.py` to generate the grading dataset and secret key.
3. Provide the grading dataset to graders along with the scoresheet templates.
4. Run `stats.py` to generate the grading statistics.

Note there are two modes in grading: `pairwise` and `single`. In the `pairwise` mode, graders are provided with pairs of completions for the same problem by the two models. In the `single` mode, graders are provided with completions from the two models for the same problem, but they are not paired. The `pairwise` mode is recommended as it allows for a more direct comparison between the two models.