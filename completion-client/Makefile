# THESE PROBABLY DON'T NEED TO BE CHANGED
NAME=completion-pod
TEMPLATE=completion-pod.yaml.mustache
MAX_TOTAL_TOKENS=8192
MAX_CONCURRET_REQUESTS=500
HOST_PORT=8080

# THESE PROBABLY DO NEED TO BE CHANGED FOR EACH EXPERIMENT
MODEL_DIR=/home/arjun/models/starcoderbase
DATA_DIR=/home/johngouwar/data/
PROMPT_FILE=prompts.jsonl
OUTPUT_FILE=outputs.jsonl
NUM_CONNECTIONS=50
NUM_RUNNERS=80
ATTEMPT_LIMIT=10
GPU=0

build:
	podman build -t local/completion-client -f Dockerfile

setup: build 
	python3 setup.py \
		--name ${NAME} \
		--template ${TEMPLATE} \
		--max-total-tokens ${MAX_TOTAL_TOKENS} \
		--max-concurrent-requests ${MAX_CONCURRET_REQUESTS} \
		--host-port ${HOST_PORT} \
		--model-dir ${MODEL_DIR} \
		--data-dir ${DATA_DIR} \
		--prompt-file ${PROMPT_FILE} \
		--output-file ${OUTPUT_FILE} \
		--num-connections ${NUM_CONNECTIONS} \
		--num-runners ${NUM_RUNNERS} \
		--attempt-limit ${ATTEMPT_LIMIT} \

run: setup
	podman play kube completion-pod.yaml


	
