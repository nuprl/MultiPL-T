#!/bin/bash
#SBATCH --nodes=1
#SBATCH --mem=40G
#SBATCH --export=ALL
#SBATCH --cpus-per-task=6
#SBATCH --job-name=train_multiplt
#SBATCH --partition=gpu
#SBATCH --gres=gpu:v100-sxm2:1
#SBATCH --output=train_%j.out
python3 train.py
