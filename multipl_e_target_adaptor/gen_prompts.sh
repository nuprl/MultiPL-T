#!/bin/bash
# Usage: gen_prompts.sh <lang> <root>
# This works best if your directory structure is like this:
# ROOT
#   MultiPL-T/
#   MultiPL-E/
# TODO: use MultiPL-E as a submodule

if [ $# -lt 3 ]; then
  echo "Usage: gen_prompts.sh <lang> <root (probably home dir)> <out> [optional: num gpus] [optional: stages to run] [optional: model name]"
    exit 1
fi

LANG=$1
ROOT=$2
OUT=$3
NUM_GPUS=${4:-1}
STAGES=${5:-"convert,translate,generate"}
MODEL_NAME=${6:-"bigcode/starcoderbase"}

# cd to directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
pushd $SCRIPT_DIR

# This generates the programs in a MultiPL-E compatible style, 
# already completed and compressed as clean-py-programs.tar.gz
if [[ $STAGES == *"convert"* ]]; then
    echo "Converting programs..."
    rm -fr ./stack-clean-python
    python3 $ROOT/MultiPL-T/multipl_e_target_adaptor/dirty_proc_dataset.py 
fi

# MultiPL-E relies a bunch on relative pathes, so it's often easier to just
# run it from the MultiPL-E directory 
# output and originals need to be relative for some reason
if [[ $STAGES == *"translate"* ]]; then
    pushd $ROOT/MultiPL-E/dataset_builder/
    echo "Translating prompts..."
    python3 prepare_prompts_json.py \
      --lang humaneval_to_$LANG.py\
      --output ../../MultiPL-T/multipl_e_target_adaptor/$LANG-prompts.jsonl \
      --originals ../../MultiPL-T/multipl_e_target_adaptor/stack-clean-python/ \
      --skip-failing-tests \
      --add-canonical-to-prompt
    popd
fi

# This actually generates the completions 
if [[ $STAGES == *"generate"* ]]; then
  pushd $ROOT/MultiPL-E/
  DATASET_LEN=$(wc -l < ../MultiPL-T/multipl_e_target_adaptor/$LANG-prompts.jsonl)
  ITEMS_PER_GPU=$((DATASET_LEN / NUM_GPUS))
  DATASET_LEN_ROUNDED=$((ITEMS_PER_GPU * NUM_GPUS))
  LEFT_OVER=$((DATASET_LEN - DATASET_LEN_ROUNDED))

  echo "[TOTAL LEN $DATASET_LEN] Using $NUM_GPUS GPUs, $ITEMS_PER_GPU items per GPU"
  
  PIDS=()

  for (( i=0; i<$NUM_GPUS; i++ ))
  do
      START_INDEX=$((i * ITEMS_PER_GPU))
      ITEMS=$((ITEMS_PER_GPU))
      # if there is leftover and this is the last gpu, add it to the items
      if [ $i -eq $((NUM_GPUS - 1)) ] && [ $LEFT_OVER -gt 0 ]; then
          echo "Adding $LEFT_OVER leftover items to GPU $i"
          ITEMS=$((ITEMS_PER_GPU + LEFT_OVER))
      fi
      echo "Starting GPU $i at $START_INDEX... Will stop at $((START_INDEX + ITEMS - 1))."
      NVIDIA_VISIBLE_DEVICES=$i CUDA_VISIBLE_DEVICES=$i python3 automodel.py \
          --name $MODEL_NAME \
          --use-local \
          --dataset ../MultiPL-T/multipl_e_target_adaptor/$LANG-prompts.jsonl \
          --completion-limit 50 \
          --batch-size 50 \
          --temperature 0.8 \
          --output-dir $OUT \
          --input-start-index $START_INDEX \
          --input-limit $ITEMS &
      PIDS+=($!)
  done

  echo "Waiting for all processes to finish... Pids: ${PIDS[@]}"

  # capture a ctrl-c and kill all processes
  function ctrl_c() {
      echo "Trapped CTRL-C, killing all processes..."
      for pid in ${PIDS[@]}; do
          kill $pid
      done
  }

  trap ctrl_c INT

  wait # wait for all background processes to finish
  popd
fi
popd
