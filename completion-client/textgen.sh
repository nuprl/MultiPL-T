#!/bin/bash
podman run \
	-p 8080:80 -v /home/arjun/models:/data \
  -e HF_HUB_ENABLE_HF_TRANSFER=0 \
	-e CUDA_VISIBLE_DEVICES=0 \
	ghcr.io/huggingface/text-generation-inference:0.9 \
	--model-id /data/starcoderbase \
  --max-total-tokens 8192 \
	--max-batch-total-tokens 128000 \
	--max-concurrent-requests 1100 \
	--waiting-served-ratio 1.7 \


