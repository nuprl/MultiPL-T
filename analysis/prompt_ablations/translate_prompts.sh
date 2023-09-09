#!/bin/bash


LANGS="ml rkt lua"

echo "NOTE: assumes that the python source files are in the MultiPL-T/stack-clean-python/ directory"
pushd ../../MultiPL-E/dataset_builder

for lang in $LANGS; do
    echo "Translating $lang"
    LANGDIR=../../analysis/prompt_ablations/canonical_comment_prompts/canonical_$lang
    PROMPTPATH=../../analysis/prompt_ablations/canonical_comment_prompts/$lang-prompts.jsonl
    python3 prepare_prompts_json.py \
      --lang humaneval_to_$lang.py\
      --output $PROMPTPATH \
      --originals ../../multipl_e_target_adaptor/stack-clean-python/ \
      --skip-failing-tests \
      --add-canonical-to-prompt

    mkdir -p $LANGDIR
done

for lang in $LANGS; do
    echo "Translating $lang"
    python3 prepare_prompts_json.py \
      --lang humaneval_to_$lang.py\
      --output ../../analysis/prompt_ablations/base_prompts/$lang-prompts.jsonl \
      --originals ../../multipl_e_target_adaptor/stack-clean-python/ \
      --skip-failing-tests
    mkdir -p ../../analysis/prompt_ablations/base_prompts/base_$lang
done


popd
