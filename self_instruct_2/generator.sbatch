#!/bin/bash
#SBATCH --nodes=1
#SBATCH --mem=64G
#SBATCH --export=ALL
#SBATCH --cpus-per-task=4
#SBATCH --job-name=multiplt_selfinstruct_generator
#SBATCH --time=18:00:00
#SBATCH --partition=177huntington
#SBATCH --gres=gpu:1
set -x
set -e
python3 generator.py

