#!/bin/bash
#SBATCH --nodes=1
#SBATCH --mem=24G
#SBATCH --export=ALL
#SBATCH --cpus-per-task=24
#SBATCH --time=0:10:00
#SBATCH --job-name=executions
#SBATCH --partition=express
#SBATCH --output=executions_%j.out

mkdir -p tmp_home
singularity exec --home tmp_home:/home/$USER \
  --network none \
  --bind $1:/dataset:rw \
   /work/arjunguha-research-group/arjun/bigcode/MultiPL-E/cluster/multipl-e-evaluation_latest.sif \
   python3 /code/main.py \
   --dir /dataset --output-dir /dataset --recursive
python3 /work/arjunguha-research-group/arjun/projects/MultiPL-T/MultiPL-E/pass_k.py $1 | tee passk.csv

