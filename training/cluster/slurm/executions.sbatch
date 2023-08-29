#!/bin/bash
#SBATCH --nodes=1
#SBATCH --job-name=spawn_executions
#SBATCH --partition=express 
#SBATCH --output=spawn_%j.out
MULTIPLE_IMAGE=$1
MULTIPLE_REPO=$2
VENV_ACTIVATE=$3
DATASET_PATH=$4
TOKENIZER_PATH=$5
for check in ./checkpoint_*; do
  #remove ./ from beginning of checkpoint name
  check_name=${check:2}
  CID=$(sbatch --parsable run_completions.sbatch $VENV_ACTIVATE $MULTIPLE_REPO $DATASET_PATH $check_name $TOKENIZER_PATH)
  sbatch --dependency=afterok:$CID eval_checkpoint.sbatch $check_name $MULTIPLE_IMAGE $MULTIPLE_REPO
done
