# THESE PROBABLY DON'T NEED TO BE CHANGED
NAME=completion-pod# what you want to call the pod
TEMPLATE=completion-pod.yaml.mustache # the template to use for kubernetes file
MAX_TOTAL_TOKENS=8192 # max total tokens for the model (context size)
MAX_CONCURRET_REQUESTS=500 # max concurrent requests for the server 
HOST_PORT=8080 # port to host the server on (not exposed to the internet)

# THESE PROBABLY DO NEED TO BE CHANGED FOR EACH EXPERIMENT
MODEL_DIR=/home/arjun/models/starcoderbase # path to the model directory on the host machine
DATA_DIR=/home/johngouwar/data/ # path to the directory where data will be stored on the host machine	
PROMPT_FILE=prompts.jsonl # name of prompt file on host machine, should be in data dir
OUTPUT_FILE=outputs.jsonl # name of output file on host machine, should be in data dir
NUM_CONNECTIONS=300 # number of connections to the server
NUM_RUNNERS=80 # number of runners to use for the evaluating completions should be <= CPUs on the host machine
ATTEMPT_LIMIT=10 # number of attempts to make to the server before giving up on a prompt
GPU=0 # which gpu to use, 0 is the first gpu, 1 is the second, etc.

run: build setup
	podman play kube ${NAME}.yaml

setup: 
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

build:
	podman build -t local/multipl-t-completion-client -f Dockerfile

	
