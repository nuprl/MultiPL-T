DATA_DIR=$1
PROMPT_FILE=$2
OUT_FILE=$3
podman run --rm -it \
    --volume $DATA_DIR:/data \
    --volume .:/output \
    --volume /tmp:/tmp \
    --network host \
    local/completion-client:latest \
    --prompt-file /data/$PROMPT_FILE \
    --output-file /output/$OUT_FILE \
    --server-url http://127.0.0.1:8080/generate \
    --num-connections 100 \
    --attempt-limit 20 \
    --proc-limit 50

