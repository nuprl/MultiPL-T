# Artifact Guide

This paper is not amenable to push-button reproduction. An attempt to reproduce everything would require over 50TB of disk space and over 500 days of GPU time (on an datacenter-grade, A100 GPU). Instead, it is better to pick a small subset of data points to reproduce. If you reproduce a fine-tuned model, you will probably want to immediately evaluate it and delete the model after the reproduction.



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
