#!/bin/bash

# assumes 8 GPUs and that all models can fit in 1 GPU
# assumes ~/MultiPL-E/prompts/humaneval-ml.jsonl exists

mkdir -p scratch


# ** nuprl/multiplcoder-34b **
# lua: b147e3321afad7c782d429abf1d333725e9809bc
# ocaml: f80fd1e9210f8a01487d284bf8dea4ed8bda20e1
# racket: ba339d056381e6ec58b0e27be2b7818fd357c722
# r: 9e5af6e7725a7466446f28d9700982d95d3e4457
# julia: 661fefef8251b4df706e198d2ef86b14acce2d84

# base model codellama/CodeLlama-34b-hf
CUDA_VISIBLE_DEVICES="0" python3 automodel_vllm.py --lang lua --output-dir ./scratch/multiplcoder_34b_lua --name nuprl/MultiPLCoder-34b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision b147e3321afad7c782d429abf1d333725e9809bc --tokenizer_name codellama/CodeLlama-34b-hf &
CUDA_VISIBLE_DEVICES="1" python3 automodel_vllm.py --lang ml --output-dir ./scratch/multiplcoder_34b_ml --name nuprl/MultiPLCoder-34b --completion-limit 100 --batch-size 100 --temperature 0.2 --revision f80fd1e9210f8a01487d284bf8dea4ed8bda20e1 --use-local --dataset ~/MultiPL-E/prompts/humaneval-ml.jsonl --tokenizer_name codellama/CodeLlama-34b-hf &
CUDA_VISIBLE_DEVICES="2" python3 automodel_vllm.py --lang rkt --output-dir ./scratch/multiplcoder_34b_rkt --name nuprl/MultiPLCoder-34b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision ba339d056381e6ec58b0e27be2b7818fd357c722 --tokenizer_name codellama/CodeLlama-34b-hf &
CUDA_VISIBLE_DEVICES="3" python3 automodel_vllm.py --lang r --output-dir ./scratch/multiplcoder_34b_r --name nuprl/MultiPLCoder-34b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision 9e5af6e7725a7466446f28d9700982d95d3e4457 --tokenizer_name codellama/CodeLlama-34b-hf &
CUDA_VISIBLE_DEVICES="4" python3 automodel_vllm.py --lang jl --output-dir ./scratch/multiplcoder_34b_jl --name nuprl/MultiPLCoder-34b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision 661fefef8251b4df706e198d2ef86b14acce2d84 --tokenizer_name codellama/CodeLlama-34b-hf &

# eval baseline

CUDA_VISIBLE_DEVICES="5" python3 automodel_vllm.py --lang lua --output-dir ./scratch/codellama_base_34b_lua --name codellama/CodeLlama-34b-hf --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
CUDA_VISIBLE_DEVICES="6" python3 automodel_vllm.py --lang ml --output-dir ./scratch/codellama_base_34b_ml --name codellama/CodeLlama-34b-hf --completion-limit 100 --batch-size 100 --temperature 0.2 --use-local --dataset ~/MultiPL-E/prompts/humaneval-ml.jsonl &
CUDA_VISIBLE_DEVICES="7" python3 automodel_vllm.py --lang rkt --output-dir ./scratch/codellama_base_34b_rkt --name codellama/CodeLlama-34b-hf --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
wait
CUDA_VISIBLE_DEVICES="0" python3 automodel_vllm.py --lang r --output-dir ./scratch/codellama_base_34b_r --name codellama/CodeLlama-34b-hf --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
CUDA_VISIBLE_DEVICES="1" python3 automodel_vllm.py --lang jl --output-dir ./scratch/codellama_base_34b_jl --name codellama/CodeLlama-34b-hf --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &

# ** nuprl/multiplcoder-1b **
# lua: 03fc93fa66ce3f3495402f5d1195aaa24d3af6e8
# ocaml: dfaca6cad71dfcd8ffb299da977bc0533ca792fc
# racket: 2cdc541bee1db4da80c0b43384b0d6a0cacca5b2
# r: 448c85dcb618982a0cc3631b0b583911665c1687
# julia: f0fa65e5f6ec51ac38e526bf1bc8e64e43617313

# base model bigcode/starcoderbase-1b

CUDA_VISIBLE_DEVICES="2" python3 automodel_vllm.py --lang lua --output-dir ./scratch/multiplcoder_1b_lua --name nuprl/MultiPLCoder-1b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision 03fc93fa66ce3f3495402f5d1195aaa24d3af6e8 --tokenizer_name bigcode/starcoderbase-1b &
CUDA_VISIBLE_DEVICES="3" python3 automodel_vllm.py --lang ml --output-dir ./scratch/multiplcoder_1b_ml --name nuprl/MultiPLCoder-1b --completion-limit 100 --batch-size 100 --temperature 0.2 --revision dfaca6cad71dfcd8ffb299da977bc0533ca792fc --use-local --dataset ~/MultiPL-E/prompts/humaneval-ml.jsonl --tokenizer_name bigcode/starcoderbase-1b &
CUDA_VISIBLE_DEVICES="4" python3 automodel_vllm.py --lang rkt --output-dir ./scratch/multiplcoder_1b_rkt --name nuprl/MultiPLCoder-1b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision 2cdc541bee1db4da80c0b43384b0d6a0cacca5b2 --tokenizer_name bigcode/starcoderbase-1b &
CUDA_VISIBLE_DEVICES="5" python3 automodel_vllm.py --lang r --output-dir ./scratch/multiplcoder_1b_r --name nuprl/MultiPLCoder-1b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision 448c85dcb618982a0cc3631b0b583911665c1687 --tokenizer_name bigcode/starcoderbase-1b &
CUDA_VISIBLE_DEVICES="6" python3 automodel_vllm.py --lang jl --output-dir ./scratch/multiplcoder_1b_jl --name nuprl/MultiPLCoder-1b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision f0fa65e5f6ec51ac38e526bf1bc8e64e43617313 --tokenizer_name bigcode/starcoderbase-1b &
CUDA_VISIBLE_DEVICES="7" python3 automodel_vllm.py --lang lua --output-dir ./scratch/starcoderbase_1b_lua --name bigcode/starcoderbase-1b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
wait
CUDA_VISIBLE_DEVICES="0" python3 automodel_vllm.py --lang ml --output-dir ./scratch/starcoderbase_1b_ml --name bigcode/starcoderbase-1b --completion-limit 100 --batch-size 100 --temperature 0.2 --use-local --dataset ~/MultiPL-E/prompts/humaneval-ml.jsonl &
CUDA_VISIBLE_DEVICES="1" python3 automodel_vllm.py --lang rkt --output-dir ./scratch/starcoderbase_1b_rkt --name bigcode/starcoderbase-1b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
CUDA_VISIBLE_DEVICES="2" python3 automodel_vllm.py --lang r --output-dir ./scratch/starcoderbase_1b_r --name bigcode/starcoderbase-1b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
CUDA_VISIBLE_DEVICES="3" python3 automodel_vllm.py --lang jl --output-dir ./scratch/starcoderbase_1b_jl --name bigcode/starcoderbase-1b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &

# ** nuprl/multiplcoder-15b **
# lua: 6069aa54dd554404dd18fccdf5dedd56b8088e74
# ocaml: e7babda985786810707200ff885df6105de7dc56
# racket: f0c77c06482f436f469007f20d731cb9dd73d609
# r: f811f0b0c0730f7964a53fd05d26235487b0277e
# julia: a08675ca659a16a0a37021a2ab74200c9e5a4966

# base model bigcode/starcoderbase

CUDA_VISIBLE_DEVICES="4" python3 automodel_vllm.py --lang lua --output-dir ./scratch/multiplcoder_15b_lua --name nuprl/MultiPLCoder-15b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision 6069aa54dd554404dd18fccdf5dedd56b8088e74 --tokenizer_name bigcode/starcoderbase &
CUDA_VISIBLE_DEVICES="5" python3 automodel_vllm.py --lang ml --output-dir ./scratch/multiplcoder_15b_ml --name nuprl/MultiPLCoder-15b --completion-limit 100 --batch-size 100 --temperature 0.2 --revision e7babda985786810707200ff885df6105de7dc56 --use-local --dataset ~/MultiPL-E/prompts/humaneval-ml.jsonl --tokenizer_name bigcode/starcoderbase &
CUDA_VISIBLE_DEVICES="6" python3 automodel_vllm.py --lang rkt --output-dir ./scratch/multiplcoder_15b_rkt --name nuprl/MultiPLCoder-15b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision f0c77c06482f436f469007f20d731cb9dd73d609 --tokenizer_name bigcode/starcoderbase &
CUDA_VISIBLE_DEVICES="7" python3 automodel_vllm.py --lang r --output-dir ./scratch/multiplcoder_15b_r --name nuprl/MultiPLCoder-15b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision f811f0b0c0730f7964a53fd05d26235487b0277e --tokenizer_name bigcode/starcoderbase &
CUDA_VISIBLE_DEVICES="0" python3 automodel_vllm.py --lang jl --output-dir ./scratch/multiplcoder_15b_jl --name nuprl/MultiPLCoder-15b --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 --revision a08675ca659a16a0a37021a2ab74200c9e5a4966 --tokenizer_name bigcode/starcoderbase &
CUDA_VISIBLE_DEVICES="1" python3 automodel_vllm.py --lang lua --output-dir ./scratch/starcoderbase_15b_lua --name bigcode/starcoderbase --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
wait

CUDA_VISIBLE_DEVICES="2" python3 automodel_vllm.py --lang ml --output-dir ./scratch/starcoderbase_15b_ml --name bigcode/starcoderbase --completion-limit 100 --batch-size 100 --temperature 0.2 --use-local --dataset ~/MultiPL-E/prompts/humaneval-ml.jsonl &
CUDA_VISIBLE_DEVICES="3" python3 automodel_vllm.py --lang rkt --output-dir ./scratch/starcoderbase_15b_rkt --name bigcode/starcoderbase --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
CUDA_VISIBLE_DEVICES="4" python3 automodel_vllm.py --lang r --output-dir ./scratch/starcoderbase_15b_r --name bigcode/starcoderbase --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
CUDA_VISIBLE_DEVICES="5" python3 automodel_vllm.py --lang jl --output-dir ./scratch/starcoderbase_15b_jl --name bigcode/starcoderbase --root-dataset humaneval --completion-limit 100 --batch-size 100 --temperature 0.2 &
wait


# eval all
dockeval () {
        docker run --rm --network none  --volume $1:/inputs:ro --volume $1:/outputs:rw multipl-e-evaluation  --dir /inputs --output-dir /outputs
}

dockeval ./scratch/multiplcoder_34b_lua
dockeval ./scratch/multiplcoder_34b_ml
dockeval ./scratch/multiplcoder_34b_rkt
dockeval ./scratch/multiplcoder_34b_r
dockeval ./scratch/multiplcoder_34b_jl
dockeval ./scratch/codellama_base_34b_lua
dockeval ./scratch/codellama_base_34b_ml
dockeval ./scratch/codellama_base_34b_rkt
dockeval ./scratch/codellama_base_34b_r
dockeval ./scratch/codellama_base_34b_jl
dockeval ./scratch/multiplcoder_1b_lua
dockeval ./scratch/multiplcoder_1b_ml
dockeval ./scratch/multiplcoder_1b_rkt
dockeval ./scratch/multiplcoder_1b_r
dockeval ./scratch/multiplcoder_1b_jl
dockeval ./scratch/starcoderbase_1b_lua
dockeval ./scratch/starcoderbase_1b_ml
dockeval ./scratch/starcoderbase_1b_rkt
dockeval ./scratch/starcoderbase_1b_r
dockeval ./scratch/starcoderbase_1b_jl
dockeval ./scratch/multiplcoder_15b_lua
dockeval ./scratch/multiplcoder_15b_ml
dockeval ./scratch/multiplcoder_15b_rkt
dockeval ./scratch/multiplcoder_15b_r
dockeval ./scratch/multiplcoder_15b_jl
dockeval ./scratch/starcoderbase_15b_lua
dockeval ./scratch/starcoderbase_15b_ml
dockeval ./scratch/starcoderbase_15b_rkt
dockeval ./scratch/starcoderbase_15b_r
dockeval ./scratch/starcoderbase_15b_jl
