#!/bin/bash
#SBATCH --nodes=1
#SBATCH --job-name=spawn_executions
#SBATCH --partition=express 
#SBATCH --output=spawn_%j
MULTIPLE_IMAGE=$1
PASS_K=$2
for check in ./checkpoint_*; do
  #remove ./ from beginning of checkpoint name
  check=${check:2}
  sbatch eval_checkpoint.sbatch $check $MULTIPLE_IMAGE $PASS_K
done
