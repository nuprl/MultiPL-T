#!/bin/bash

set -x
set -e
DIR=`dirname $0`

TRAIN_JOBID=`sbatch $DIR/train.sbatch $1 | awk '{print $4}'`
EXECUTIONS_JOBID=`sbatch --dependency=afterok:$TRAIN_JOBID $DIR/executions.sbatch ./checkpoint_final/eval | awk '{print $4}'`

echo "Submitted training job $TRAIN_JOBID"
echo "Submitted executions job $EXECUTIONS_JOBID"