PASS_K=/home/johngouwar/repos/MultiPL-E/pass_k.py

CUDA_VISIBLE_DEVICES=0 python3 train.py 

for $check in ./checkpoint*; do
    # Run the evaluation
    check_name=$(basename $check)
    # Find directory in check with prefix eval
    eval_dir=$(find $check -type d -name "eval*")
    mkdir -p tmp_home 
    podman run \
      --network none \
      --volume $eval_dir:/dataset:rw \
      multipl-e-evaluation:latest \
      --dir /dataset --output-dir /dataset --recursive
    python3 $PASS_K $check/eval | tee passk_$check_name.csv
done