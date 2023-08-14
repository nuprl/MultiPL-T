EXP_ROOT="/home/johngouwar/experiments/8_14_multiplt_mixed_training"
DATA_DIR="/home/johngouwar/data/8_14_multiplt_mixed_training"
BASE_CMD="python3 build_train_experiment.py \
    --train-template simple-train-py.mustache \
    --test-data $DATA_DIR/lua_rkt_ml_prompts.jsonl \
    --exp-root $EXP_ROOT \
    --learning-rate 3e-5 \
    --batch-size 8 \
    --schedule cosine \
    --epochs 4 \
    --warmup-steps 10 \
    --single"

# Build train.py and directory structure
$BASE_CMD --exp-name lua_rkt_ml_equal --training-items 121530 --train-data $DATA_DIR/lua_rkt_ocaml_equal_train.jsonl
$BASE_CMD --exp-name lua_rkt_ml_unequal --training-items 132105 --train-data $DATA_DIR/lua_rkt_ocaml_unequal_train.jsonl