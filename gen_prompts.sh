# Usage: gen_prompts.sh <lang> <root>
# This works best if your directory structure is like this:
# ROOT
#   multipl-t/
#   MultiPL-E/
LANG=rkt
ROOT=/home/jgouwar/Git/research
OUT=fixed-rkt-prompts.json

# This generates the programs in a MultiPL-E compatible style, 
# already completed and compressed as clean-py-programs.tar.gz
python3 $ROOT/multipl-t/src/dirty_proc_dataset.py 

# MultiPL-E relies a bunch on relative pathes, so it's often easier to just
# run it from the MultiPL-E directory 
# output and originals need to be relative for some reason
cd $ROOT/MultiPL-E/dataset_builder/
python3 prepare_prompts_json.py \
    --lang humaneval_to_$LANG.py\
    --output ../../multipl-t/$LANG-prompts.jsonl\
    --originals ../../multipl-t/stack-clean-python/

