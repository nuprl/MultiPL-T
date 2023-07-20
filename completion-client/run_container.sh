PROMPT_FILE=$1
OUT_FILE=$2
NUM_CONNECTIONS=$3

#Validate the input to the script has 3 arguments
if [ $# -ne 3 ]; then
    echo "Usage: $0 <prompt_file> <out_file> <num_connections>"
    exit 1
fi



podman run -it --rm \
	--volume $(PROMPT_FILE):/prompts.jsonl:ro \
	--volume $(OUT_FILE):/out.jsonl \
	local/completion-client \
	--prompt-file prompts.jsonl \
	--out-file out.jsonl \
	--num-connections $NUM_CONNECTIONS \
    --server-url http://127.0.0.1:8080/generate 