#!/bin/bash


LANGS="ml rkt lua"

echo "NOTE: assumes that the prompts have been created via ./translate_prompts.sh"
pushd ../../MultiPL-E

for lang in $LANGS; do
  echo "Translating $lang"
  BASE_LANGDIR=../analysis/prompt_ablations/base_prompts/$lang
  BASE_PROMPTPATH=../analysis/prompt_ablations/base_prompts/$lang-prompts.jsonl
  echo "Base lang dir: $BASE_LANGDIR"
  python3 automodel.py \
  --name "bigcode/starcoderbase" \
  --use-local \
  --dataset $BASE_PROMPTPATH \
  --completion-limit 50 \
  --batch-size 50 \
  --temperature 0.8 \
  --output-dir $BASE_LANGDIR

  PROMPT_LANGDIR=../analysis/prompt_ablations/canonical_comment_prompts/$lang
  PROMPT_PROMPTPATH=../analysis/prompt_ablations/canonical_comment_prompts/$lang-prompts.jsonl
  echo "Prompt lang dir: $PROMPT_LANGDIR"
  python3 automodel.py \
  --name "bigcode/starcoderbase" \
  --use-local \
  --dataset $PROMPT_PROMPTPATH \
  --completion-limit 50 \
  --batch-size 50 \
  --temperature 0.8 \
  --output-dir $PROMPT_LANGDIR
done
popd
