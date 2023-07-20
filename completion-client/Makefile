build:
	podman build -t local/completion-client .

test: build
	./run_container.sh small.jsonl test-out.jsonl 50	