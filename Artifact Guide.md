# Artifact Guide for MultiPL-T

## Introduction

We can break MultiPL-T down into the following steps:

1. Filter Python from *The Stack* to a high-quality subset of Python
   examples, which includes LLM-generated tests that are validated by execution
   (Sections 4.1 and 4.2).
2. Use LLM to translate the Python examples to a low-resource
   programming language. Filter out incorrect translations using test cases
   translated with MultiPL-E. We support translation to Racket, Julia, R, OCaml,
   and Lua (Section 4.3 and 4.4).
3. Fine-tune off-the-shelf LLMs on each low-resource language. (Fine-tuning
   hyperparameters are described at the top of Section 5.)
4. Evaluate the performance of these fine-tuned LLMs and compare to baselines
   (Section 5).

The paper has several secondary ablations and evaluations, but the steps
above describe the primary artifact.

All the steps above require a GPU. Moreover, as the paper reports, doing a
complete reproduction will require at least 550 days on a datacenter GPU
([A100]), and several times longer on a consumer GPU. We have provided
pre-built artifacts:

1. The filtered Python subset of The Stack (Step 1 above):
 
   https://huggingface.co/datasets/nuprl/stack-dedup-python-testgen-starcoder-filter-v2

   We recommend *not* attempting to rebuild this dataset. We estimate
   this required 2,000 hours on H100 / A100 GPUs.

2. The MultiPL-T fine-tuning datasets for the five programming languages:

   https://huggingface.co/datasets/nuprl/MultiPL-T

   We recommend *not* attempting to rebuild this dataset. We estimate that 
   translating each language takes approximately 1,400 hours on an A100 GPU. 

3. Fine-tuned off-the-shelf LLMs for each low-resource language. The resources
   needed to fine-tune an LLM vary significantly based on the LLM size.
   The MultiPL-T dataset is small enough that one can fine-tune StarCoderBase-1B
   in a few hours on an A100. However, the larger models require several days
   and also require 4-8 A100 GPUs.

   Our fine-tuned models are available in this collection:

   https://huggingface.co/collections/nuprl/multipl-t-65242261eadae29c5faab50e

   We describe them in more detail below.

## Recommendation for Artifact Evaluation

- Given a recent consumer Nvidia GPU, such as an RTX 30xx or 40xx, with 11GB+
  VRAM, it should be possible to re-evaluate StarCoderBase-1b. It will not be
  possible to fine-tune models on these GPUs.

- Given an Nvidia GPU with 40GB VRAM, such as an A6000 or older A100, it will 
  also be possible to fine-tune StarCoderBase-1b. *We will attempt to provide
  SSH access to a 40GB A100 for artifact evaluation.*

- On an 80GB 8xA100 node, it is possible to do reproduce any part of the
  evaluation, but is very expensive.


## Figure 3a

**Fine-tuning on the complete language-specific subsets of The Stack.**

All of these fine-tuned models are available at https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_1b
and are tagged `$LANG-morestack-epoch_$N`. Run this command to see the list:

```bash
huggingface-cli tag -l nuprl/MultiPL-T-StarCoderBase_1b | grep morestack
```

## Figure 3b

**Fine-tuning on the token-balanced language-specific subsets of The Stack.**

All of these fine-tuned models are available at https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_1b
and are tagged `$LANG-balancedstack-epoch_$N`. Run this command to see the list:

```bash
huggingface-cli tag -l nuprl/MultiPL-T-StarCoderBase_1b | grep balancedstack
```

## Figure 8

**We fine-tune three versions of StarCoderBase-1B on 25k MultiPL-T generated training items.**

All of these fine-tuned models are available at https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_1b
and are tagged `$LANG-25k-epoch$N`. Run this command to see the list:

```bash
huggingface-cli tag -l nuprl/MultiPL-T-StarCoderBase_1b | grep 25k
```

## Table 2

**MultiPL-E pass@1 scores for 1B, 15B, 34B, and 70B parameter models before and after fine-tuned on MultiPL-T data.**

The base models are third-party models. The fine-tuned models are available in the repositories below.

- [nuprl/MultiPL-T-StarCoderBase_1b](https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_1b)
- [nuprl/MultiPL-T-StarCoderBase_15b](https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_15b)
- [nuprl/MultiPL-T-CodeLlama_34b](https://huggingface.co/nuprl/MultiPL-T-CodeLlama_34b)

  ```bash
  huggingface-cli tag -l nuprl/MultiPL-T-CodeLlama_34b
  ```

  
- [nuprl/MultiPL-T-CodeLlama_70b](https://huggingface.co/nuprl/MultiPL-T-CodeLlama_70b)

  Each model requires ~280GB disk space. We saved the best performing checkpoints for
  each programming language. (We had to evaluate and delete the others during training
  to manage disk space on our GPU server.)

  ```bash
  huggingface-cli tag -l nuprl/MultiPL-T-CodeLlama_70b
  ```

## Table 3

**MultiPL-E pass@1 scores for two recently released models on Racket.**

The base models are third-party models. The fine-tuned models are available in the repositories below.

- [nuprl/MultiPL-T-StarCoder2_15B](https://huggingface.co/nuprl/MultiPL-T-StarCoder2_15B)
- [nuprl/MultiPL-T-DeepSeekCoder_33b](https://huggingface.co/nuprl/MultiPL-T-DeepSeekCoder_33b)

## Table 6

**Dataset sizes**

The reported sizes in this table are the sizes of each split in the [nuprl/MultiPL-T](https://huggingface.co/datasets/nuprl/MultiPL-T) dataset.

## Appendix A

**A Full Self-Instruction Experiment**

See the directory `./A_A_Full_Self_Instruction-Experiment` in this repository.

[A100]: https://www.nvidia.com/en-us/data-center/a100/
