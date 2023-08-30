#!/bin/bash
#SBATCH --nodes=1
#SBATCH --mem=40G
#SBATCH --export=ALL
#SBATCH --cpus-per-task=6
#SBATCH --job-name=compl
#SBATCH --partition=gpu
#SBATCH --gres=gpu:v100-sxm2:1
#SBATCH --output=compl_%j.out
VENV_ACTIVATE=$1
MULTIPLE_REPO_PATH=$2
DATASET_PATH=$3
MODEL=$4
TOKENIZER=$5
SCRIPT_PATH="$MULTIPLE_REPO_PATH/automodel.py"

module unload 
module load python
source $VENV_ACTIVATE
python3 $SCRIPT_PATH \
    --name $MODEL \
    --use-local \
    --dataset $DATASET_PATH \
    --batch-size 40 \
    --temperature 0.2 \
    --completion-limit 20 \
    --output-dir "$MODEL/completions"\
    --tokenizer_name $TOKENIZER