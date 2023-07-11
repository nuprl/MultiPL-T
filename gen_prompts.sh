# Usage: gen_prompts.sh <lang> <root>
# This works best if your directory structure is like this:
# ROOT
#   multipl-t/
#   MultiPL-E/

if [ $# -lt 3 ]; then
  echo "Usage: gen_prompts.sh <lang> <root (probably home dir)> <out> [optional: num gpus] [optional: stages to run]"
    exit 1
fi

LANG=$1
ROOT=$2
OUT=$3
NUM_GPUS=${4:-1}
STAGES=${5:-"convert,translate,generate"}

# This generates the programs in a MultiPL-E compatible style, 
# already completed and compressed as clean-py-programs.tar.gz
if [[ $STAGES == *"convert"* ]]; then
    echo "Converting programs..."
    python3 $ROOT/multipl-t/src/dirty_proc_dataset.py 
fi

# MultiPL-E relies a bunch on relative pathes, so it's often easier to just
# run it from the MultiPL-E directory 
# output and originals need to be relative for some reason
if [[ $STAGES == *"translate"* ]]; then
    pushd $ROOT/MultiPL-E/dataset_builder/
    echo "Translating prompts..."
    python3 prepare_prompts_json.py \
      --lang humaneval_to_$LANG.py\
      --output ../../multipl-t/$LANG-prompts.jsonl \
      --originals ../../multipl-t/stack-clean-python/
    popd
fi

# This actually generates the completions 
if [[ $STAGES == *"generate"* ]]; then
  pushd $ROOT/MultiPL-E/
  DATASET_LEN=$(wc -l < ../multipl-t/$LANG-prompts.jsonl)
  ITEMS_PER_GPU=$((DATASET_LEN / NUM_GPUS))
  echo "[TOTAL LEN $DATASET_LEN] Using $NUM_GPUS GPUs, $ITEMS_PER_GPU items per GPU"
  
  PIDS=()

  for (( i=0; i<$NUM_GPUS; i++ ))
  do
      START_INDEX=$((i * ITEMS_PER_GPU))
      echo "Starting GPU $i at $START_INDEX... Will stop at $((START_INDEX + ITEMS_PER_GPU - 1))."
      NVIDIA_VISIBLE_DEVICES=$i python3 automodel.py \
          --name /home/arjun/models/starcoderbase \
          --use-local \
          --dataset ../multipl-t/$LANG-prompts.jsonl \
          --completion-limit 50 \
          --batch-size 50 \
          --temperature 0.8 \
          --output-dir $OUT \
          --input-start-index $START_INDEX \
          --input-limit $ITEMS_PER_GPU &
      PIDS+=($!)
  done

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
