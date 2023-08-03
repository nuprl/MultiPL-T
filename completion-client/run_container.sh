CLIENT=local/rs-completion-client:latest
DATA_DIR=$1
PROMPT_FILE=$2
OUT_FILE=$3
LOG_FILE=log.out
podman run --rm -it \
    --volume $DATA_DIR:/data \
    --volume .:/output \
    --volume /tmp:/tmp \
    --network host \
    $CLIENT \
    --prompt-file /data/$PROMPT_FILE \
    --output-file /output/$OUT_FILE \
    --log-file /output/$LOG_FILE \
    --endpoint-url http://127.0.0.1:8080/generate \
    --num-connections 100 \
    --attempt-limit 20 \
    --num-runners 50

