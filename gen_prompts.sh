ROOT="/home/jgouwar/Git/research"
python3 $ROOT/multipl-t/src/dirty_proc_dataset.py 

cd $ROOT/MultiPL-E/dataset_builder/
python3 prepare_prompts_json.py \
    --lang humaneval_to_rkt.py\
    --output ../../multipl-t/rkt-prompts.json\
    --originals ../../multipl-t/stack-clean-python/