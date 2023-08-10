#!/bin/bash
EXP_ROOT="/home/johngouwar/experiments/8_10_multiplt_training"
DATA_DIR="/home/johngouwar/data/multiplt-training"
BASE_CMD="python3 build_train_experiment.py \
    --train-template simple-train-py.mustache \
    --train-data $DATA_DIR/ocaml_full_train_29674.jsonl \
    --test-data $DATA_DIR/humaneval-ml-reworded.jsonl \
    --exp-root $EXP_ROOT \
    --learning-rate 3e-5 \
    --batch-size 8 \
    --schedule cosine \
    --epochs 6 \
    --warmup-steps 10 \
    --single" 

# Build train.py and directory structure 
$BASE_CMD --exp-name ocaml_5k --training-items 5000
$BASE_CMD --exp-name ocaml_10k --training-items 10000
$BASE_CMD --exp-name ocaml_15k --training-items 15000
$BASE_CMD --exp-name ocaml_20k --training-items 20000
$BASE_CMD --exp-name ocaml_25k --training-items 25000
$BASE_CMD --exp-name ocaml_full --training-items 29674

# Copy run_exp.sh to each experiment directory
for exp in $EXP_ROOT/*; do
    if [ ! -d $exp ]; then
        continue
    fi
    cp run_exp.sh $exp
done

