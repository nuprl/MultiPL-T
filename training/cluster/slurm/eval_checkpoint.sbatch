#!/bin/bash
#SBATCH --nodes=1
#SBATCH --mem=24G
#SBATCH --export=ALL
#SBATCH --cpus-per-task=24
#SBATCH --time=0:15:00
#SBATCH --job-name=executions
#SBATCH --partition=express
#SBATCH --output=eval_%j.out

# $1: path to checkpoint
# $2: path to MultiPL-E singularity image
# $3: path to pass_k.py

CHECKPOINT=$1
MULTIPLE_IMAGE=$2
MULTIPLE_REPO=$3
PASS_K="$MULTIPLE_REPO/pass_k.py"

module unload 
module load singularity
module load python
source ~/work/gouwar.j/venv/bin/activate
mkdir -p tmp_home
singularity exec --home tmp_home:/home/$USER \
  --network none \
  --bind $CHECKPOINT/completions:/dataset:rw \
  $MULTIPLE_IMAGE \
  python3 /code/main.py \
  --dir /dataset --output-dir /dataset --recursive
python3 $PASS_K $CHECKPOINT/eval | tee passk_$CHECKPOINT.csv 

