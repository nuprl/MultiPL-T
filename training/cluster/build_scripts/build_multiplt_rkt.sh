#!/bin/bash
EXP_ROOT="/home/johngouwar/experiments/8_10_multiplt_training"
DATA_DIR="/home/johngouwar/data/multiplt-training"
BASE_CMD="python3 build_train_experiment.py \
    --train-template rkt-train-py.mustache \
    --train-data $DATA_DIR/racket_full_train_40510.jsonl \
    --test-data $DATA_DIR/humaneval-rkt-reworded.jsonl \
    --exp-root $EXP_ROOT \
    --learning-rate 3e-5 \
    --batch-size 8 \
    --schedule cosine \
    --epochs 6 \
    --warmup-steps 10 \
    --single" 

# Build train.py and directory structure 
$BASE_CMD --exp-name rkt_5k --training-items 5000
$BASE_CMD --exp-name rkt_10k --training-items 10000
$BASE_CMD --exp-name rkt_15k --training-items 15000
$BASE_CMD --exp-name rkt_20k --training-items 20000
$BASE_CMD --exp-name rkt_25k --training-items 25000
$BASE_CMD --exp-name rkt_30k --training-items 30000
$BASE_CMD --exp-name rkt_35k --training-items 35000
$BASE_CMD --exp-name rkt_full --training-items 40510

# Copy run_exp.sh to each experiment directory
for exp in $EXP_ROOT/*; do
    if [ ! -d $exp ]; then
        continue
    fi
    cp run_exp.sh $exp
done

