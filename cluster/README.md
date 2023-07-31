# Running simple finetuning experiments on StarCoderBase-1b

## Setting up requirements 

Activate your python venv and make sure you have the requirements specified in 
`requirements.txt`
    [gouwar.j@login-01 ~]$: source venv/bin/activate
    [gouwar.j@login-01 ~]$: pip install -r requirements.txt

## train_eval

### The Training Script 

The `train.py` script runs the simple trainer from the `code_llms` "package." 
Edit this script to set the hyperparameters and describe how to load the train 
and test data. 

### The Launch Script 
 
`launch.sh` configures how the experiment is run. It has 4 paths that need to be 
set: 
- The location of the MultiPL-E singularity container 
- The location of the MultiPL-E `pass_k.py` script
- The location of the `activate` script to activate a suitable python venv 
- The location of the `HF_DATASETS_CACHE`

Setting the 4 variables at the top of the script launches the training run described 
in `train.py` on a V100 node, spawns an evaluation job for each checkpoint, and 
then runs the `pass_k.py` script on the evaluation results. 

## inf_eval