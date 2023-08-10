#!/bin/bash
BASE_CMD="python3 build_train_experiment.py \
    --train-template rkt-train-py.mustache \
    --train-data racket_full_train_40510.jsonl \
    --test-data humaneval_rkt_reworded.jsonl \
    --exp-root 8_10_multiplt_training \
    --learning-rate 3e-5 \
    --batch-size 8 \
    --schedule cosine \
    --epochs 6 \
    --warmup-steps 10 \
    --single" 

$BASE_CMD --exp-name rkt_5k --training-items 5000
$BASE_CMD --exp-name rkt_10k --training-items 10000
$BASE_CMD --exp-name rkt_15k --training-items 15000
$BASE_CMD --exp-name rkt_20k --training-items 20000
$BASE_CMD --exp-name rkt_25k --training-items 25000
$BASE_CMD --exp-name rkt_30k --training-items 30000
$BASE_CMD --exp-name rkt_35k --training-items 35000
$BASE_CMD --exp-name rkt_full --training-items 40510

