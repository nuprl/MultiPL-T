$PASS_K=/home/johngouwar/repos/MultiPL-E/pass_k.py

CUDA_VISIBLE_DEVICES=0 python3 train.py 

for $check in ./checkpoint*; do
    # Run the evaluation
    check_name=$(basename $check)
    mkdir -p tmp_home 
    podman run --home tmp_home:/home/$USER \
      --network none \
      --volume $check/eval:/dataset:rw \
      multilpl-e-evalution:latest \
      --dir /dataset --output-dir /dataset --recursive
    python3 $PASS_K $check/eval | tee passk_$check_name.csv
done