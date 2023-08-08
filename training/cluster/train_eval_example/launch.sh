#!/bin/bash
# This script runs a training job and then submits an execution job for each
# checkpoint at the end of training. Make sure to provide a train.py for this
# experiment. 

MULTIPLE_IMAGE="/work/arjunguha-research-group/gouwar.j/multipl-e-evaluation_latest.sif"
PASS_K="/work/arjunguha-research-group/arjun/projects/MultiPL-T/MultiPL-E/pass_k.py" 
VENV_ACTIVATE="/work/arjunguha-research-group/gouwar.j/venv/bin/activate" 
HF_CACHE="/work/arjunguha-research-group/gouwar.j/cache"

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
sbatch --dependency=afterok:$TID executions.sbatch $MULTIPLE_IMAGE $PASS_K 


