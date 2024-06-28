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

## Hardware Dependencies

### Minimum Requirements

1. A recent consumer Nvidia GPU, such as an RTX 30xx or RTX 40xx
2. At least 40GB of free disk space to install PyTorch, download LLMs, etc.
3. Linux or Windows Subsystem for Linux (WSL2). **MacOS will not work.**

### What Can Be Evaluated?

- Given a recent consumer Nvidia GPU, such as an RTX 30xx or 40xx, with 11GB+
  VRAM, it should be possible to re-evaluate StarCoderBase-1b. It will not be
  possible to fine-tune models on these GPUs.

- Given an Nvidia GPU with 40GB VRAM, such as an A6000 or older A100, it will 
  also be possible to fine-tune StarCoderBase-1b. *We will attempt to provide
  SSH access to a 40GB A100 for artifact evaluation.*

- On an 80GB 8xA100 node, it is possible to do reproduce any part of the
  evaluation, but is very expensive.

## Getting Started Guide

Please complete *Installation* and *Evaluate a Base Model* for the
kick-the-tires phase.

### Installation

It is fairly standard for SIGPLAN artifacts to be packed in a container
or VM, so that the committee does not need to bother with installation.
Unfortunately:

- Getting a GPU to work with a VM/container is extraordinarily
  complicated.
   
- The software stack that you install will depend on the GPU that you have
  available.
  
- We would need to run a container-in-a-container for evaluation, which is
  another can of worms.
  
Instead, we will guide you through installing a toolchain that works for your
hardware.

1. Basic requirements:

   a. You need to be on Linux or the Windows Subsystem for Linux (WSL2).
   
   b. You need at Python 3.10 or higher. run `python3 --version` to check your
      Python version.
   
   c. You need an Nvidia GPU with 10GB+ VRAM and CUDa 12.x (preferred) or CUDA 11.8.
      Check your VRAM and CUDA version by running `nvidia-smi`.

   d. You need the ability to run a container, e.g., using Docker or Podman.

2. *Recommended:* Create and activate a new Python virtual environment:

   ```bash
   cd ~
   python3 -m venv multiplt     # Creates the environment
   source multiplt/bin/activate # Activates the environment
   ```

   - If activation succeeds, your CLI prompt should be prefixed with `(multiplt)`.
     **For the rest of this guide, we will assume that you're running commands
     in this virtual environment.**
   
   - Creating the environment may fail if you don't have the right dependency
     installed. If that occurs, follow the directions printed on screen to
     install the right package for your system.

3. Install PyTorch:
   
   - If you have CUDA 12.1+, you can run `pip3 install torch`.
   - Otherwise, see https://pytorch.org for guidance.
   
4. Verify that PyTorch is installed and correctly detects your GPU.

   Start a Python REPL (`python3`) and enter the following:

   ```python
   import torch
   torch.cuda.is_available()
   ```

   You should see `True`. Type `exit()` to quit the Python REPL.

5. Install other needed packages:

  ```bash
  pip3 install transformers datasets accelerate
  ```

6. Checkout MultiPL-E:

   ```bash
   git clone -b StarCoder2 https://github.com/nuprl/MultiPL-E.git
   ```

7. Download the MultiPL-E Evaluation container:

   ```bash
   podman pull ghcr.io/nuprl/multipl-e-evaluation
   ```

### Evaluate a Base Model

### Step 1

Before trying to evaluate a model fine-tuned with MultiPL-T, we recommend
evaluating a base model from the StarCoder or Code Llama family. Unfortunately,
to use these models, you need to create an account on huggingface.co and agree
to there terms of use. Moreover, Code Llama requires someone at Meta to
manually approve your application.

However, we have a copy of StarCoderBase-1b available that doesn't an
account. You can download it as follows:

```bash
huggingface-cli download arjunguha/notstarcoderbase-1b
```

### Step 2

Use MultiPL-E to generate completions (i.e., code) using the downloaded model:

1. Ensure you're in the MultiPL-E directory that you checked out earlier:

   ```bash
   cd MultiPL-E
   ```

2. Generate Racket completions:

   ```bash
   python3 automodel.py --name arjunguha/notstarcoderbase-1b \
    --root-dataset humaneval \
    --lang rkt \
    --temperature 0.2 \
    --completion-limit 20 \
    --output-dir out \
    --batch-size 40
   ```

   This will load the model to GPU and start generating results in the `out/`
   directory. On an RTX 3080, this will take ~5m to run. A few notes and
   recommendations:

   - The program will resume if an error occurs, such as a CUDA out-of-memory
     error. So, do not delete the `out` directory unless necessary.
   - You can monitor GPU memory usage using `nvidia-smi`. If memory usage is
     too low, you can increase the `--batch-size`.
   - Conversely, you can decrease `--batch-size` if you get a CUDA out-of-memory
     error.
    

3. Execute the generated completions:

   ```bash
   podman run --rm --network none -v ./out:/out:rw ghcr.io/nuprl/multipl-e-evaluation \
    --dir /out --output-dir out
  ```

  This process is CPU intensive and takes about 15 minutes on a 20-core Intel
  Core i9-10900KF.

4. Compute the pass rate (pass@1):

   ```bash
   python3 pass_k ./out
   ```

   You should see something like this:

   ```
   Dataset,Pass@k,Estimate,NumProblems,MinCompletions,MaxCompletions
   out,1,0.04,161,20,20
   ```

   Here is how to read it:

   - `out`: the name of the directory
   - `1`: This is pass@1, as opposed to pass@10 or pass@100
   - `0.04`: This is the pass rate (**4.4%**)
   - `161`: The number of problems evaluated. For Racket, it should be 161. It 
     is slightly lower for the other languages.
  - `20,20`: the minimum and maximum number of completions per problem. Since,
    we ran with `--num-completions 20` earlier, both should be 20. If the minimum
    is lower, either completions or executions were interrupted. You can run them
    again to continue.

5. Cross-check the pass rate with the pass rate in the paper. Table 2 lists the
   pass rate on Racket for StarCoderBase-1b as **4.7%**. We are using a
   standard, non-deterministic, sampling based LLM generation algorithm, and this
   is close enough. You can get a more stable estimate with `--num-completions 200`,
   but it will take 10x longer.

Congratulations if you made it this far! Evaluating fine-tuned MultiPL-T
models is not very different from evaluating a base model.

## Step by Step Instructions

### Evaluating a Fine-Tuned Model

Evaluating a fine-tuned model is not very different from evaluating a base
model. Every model has two pieces:

1. The tokenizer: MultiPL-T does not change this. Thus when generating completions,
   you will need to point the model to the base model's tokenizer, as described
   below.

2. The model itself: this is fine-tuned. All you need to know is the repository
   and tag where the model is stored.




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
