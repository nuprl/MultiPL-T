#!/bin/bash
# $1: GPUS, comma separated
if [ -z "$1" ]; then
    echo "Usage: $0 <GPUS>"
    exit 1
fi

GPUS=$1
NUM_GPUS=$(echo $GPUS | tr "," "\n" | wc -l)
echo "Using $NUM_GPUS GPUs: $GPUS"

PIDS=()

# trap ctrl-c and call ctrl_c() to kill all processes
function ctrl_c() {
    echo "** Trapped CTRL-C"
    for pid in ${PIDS[@]}; do
        kill $pid
    done
    exit 1
}
trap ctrl_c INT

# get rest of args after $1
shift

for i in $(seq 0 $((NUM_GPUS-1))); do
    GPU=$(echo $GPUS | cut -d "," -f $((i+1)))
    echo "Starting GPU $GPU with args: $@"
    CUDA_VISIBLE_DEVICES=$GPU WORLD_SIZE=$NUM_GPUS RANK=$i python -u $@ &
    PIDS+=($!)
done
