#!/bin/bash
# This script runs a training job and then submits an execution job for each
# checkpoint at the end of training. Make sure to provide a train.py for this
# experiment. 

$MULTIPLE_IMAGE=0 #replace with path to singularity image
$PASS_K=0 #replace with path to pass_k.py
$VENV_ACTIVATE=0 #replace with path to venv/bin/activate
$HF_CACHE="" #replace with path to huggingface cache

# Check that MUTLIPLE_IMAGE and PASS_K are set
if [ -z "$MULTIPLE_IMAGE" ]; then
  echo "MULTIPLE_IMAGE is not set"
  exit 1
fi
if [ -z "$PASS_K" ]; then
  echo "PASS_K is not set"
  exit 1
fi
if [ -z "$VENV_ACTIVATE" ]; then
  echo "VENV_ACTIVATE is not set"
  exit 1
fi

TID=$(sbatch --parsable train.sbatch $VENV_ACTIVATE $HF_CACHE)
sbatch executions.sbatch --dependency=afterok:$TID $MULTIPLE_IMAGE $PASS_K