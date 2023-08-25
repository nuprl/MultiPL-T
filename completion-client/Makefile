build:
	podman build -t local/rs-completion-client -f Dockerfile.rust

setup: build 
	python3 setup.py \
		--name test-completion-pod \
		--model-dir /home/arjun/models/starcoderbase \
		--data-dir /home/johngouwar/data/8_25_kubernetes_fun \
		--prompt-file prompts.jsonl \
		--output-file outputs.jsonl \
		--num-connections 50 \
		--num-runners 80 \
		--attempt-limit 10 \

run: setup
	podman play kube completion-pod.yaml


	
