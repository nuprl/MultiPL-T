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
  pip3 install 'huggingface_hub[cli]'
  ```

6. Checkout the MultiPL-E source code:

   ```bash
   git clone -b StarCoder2 https://github.com/nuprl/MultiPL-E.git
   ```

7. Download the MultiPL-E container:

   ```bash
   docker pull ghcr.io/nuprl/multipl-e-evaluation
   ```

   (You can use `podman` instead of `docker` above.)

### Evaluate a Base Model

Before trying to evaluate a model fine-tuned with MultiPL-T, we recommend
evaluating a base model from the StarCoder or Code Llama family. Unfortunately,
to use these models, you need to create an account on huggingface.co and agree
to their terms of use. Moreover, Code Llama requires someone at Meta to
manually approve your application.

However, we have a copy of StarCoderBase-1b available that doesn't an
account. We wil walk you through evaluating this model.

1. Download the model.

  ```bash
  huggingface-cli download arjunguha/notstarcoderbase-1b
  ```
2. Generate completions with MultiPL-E. First, ensure you are in the MultiPL-E
   directory that you checked out earlier during Installation.
  
   ```bash
   cd MultiPL-E
   ```

   Now, generate Racket completions:

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

   - You can monitor GPU memory usage using `nvidia-smi`. If memory usage is
     too low, you can increase the `--batch-size`.
   - Conversely, you can decrease `--batch-size` if you get a CUDA out-of-memory
     error.
   - If you restart, MultiPL-E will not regenerate completions that are already
     saved. If you really want to regenerate completions, you can delete
     `out/*.json.gz`.
    

3. Execute the generated completions with MultiPL-E.

   ```bash
   podman run --rm --network none -v ./out:/out:rw ghcr.io/nuprl/multipl-e-evaluation \
    --dir /out --output-dir out
  ```

  A few notes:

  - This process is CPU intensive and takes about 15 minutes on a 20-core Intel
    Core i9-10900KF.
  - This command saves execution results to the `./out` directory, alongside
    the completions.
   - If you restart, MultiPL-E will not re-execute completions that it has
     already run.. If you really want to re-execute completions, you can delete
     `out/*.results.json.gz`.

4. Compute the pass rate (pass@1).

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

6. *Optional*. Recover some disk space.
   
   - Once you're happy with the results, you can delete the `./out` directory, 
     or rename it to something more meaningful.
  - The model consumes 5GB of disk space, and you probably want to recover it.
    To do so, run `huggingface-cli delete-cache`. You'll get a textual UI
    where you can press *space* to select the model to delete and *enter* to
    actually delete it.

Congratulations if you made it this far! Evaluating fine-tuned MultiPL-T
models is not very different from evaluating a base model.

## Step by Step Instructions

### Evaluating a Fine-Tuned Model

Evaluating a fine-tuned model is not very different from evaluating a base
model, as described in the *Getting Started Guide.* The only difference is that
you need to specify two pieces of information when generating completions:

1. The location of the model, which we described below; and
2. The location of the model's tokenizer, which is the location of the original
   model.

Since we evaluated StarCoderBase-1b on Racket in the Getting Started Guide, we
will do a walkthrough of the Racket fine-tuned version of the same model. This
model's performance is reported in Table 2 as **11.3%**, and we will now
reproduce this number.

1. Download the model.

   ```bash
   huggingface-cli download nuprl/MultiPL-T-StarCoderBase_1b --revision rkt-multiplt-epoch5
   ```

   We will explain the naming scheme for the fine-tuned models later.

2. Generate Racket completions with MultiPL-E.

   ```bash
   python3 automodel.py \
     --name nuprl/MultiPL-T-StarCoderBase_1b \
     --revision rkt-multiplt-epoch5 \
     --tokenizer_name arjunguha/notstarcoderbase-1b \
     --root-dataset humaneval \
     --lang rkt \
     --temperature 0.2 \
     --completion-limit 20 \
     --output-dir out \
     --batch-size 40
   ```

   Notice how this command slightly is different from the way we evaluated the
   base model. In addition to specifying the model name, we also specified the
   `--revision` and `--tokenizer_name` flags.

3. Execute the generated completions with MultiPL-E.

   ```bash
   docker run --rm --network none -v ./out:/out:rw ghcr.io/nuprl/multipl-e-evaluation \
    --dir /out --output-dir out
   ```

   This step is unchanged from the Getting Started Guide. In fact, it is the
   same for every model and programming language. The completions include the 
   PL name, and the container packages the runtimes for all MultiPL-E supported
   PLs.

4. Compute the pass rate (pass@1).

   ```bash
   python3 pass_k ./out
   ```

   See the Getting Started Guide for directions on how to read this output.

5. When complete, delete the downloaded model to recover disk space:

   ```bash
   huggingface-cli delete-cache
   ```

### Evaluate Other Fine-Tuned Models

*What is available?* We have saved checkpoints for every fine-tuned model
discussed in the paper. We have also include checkpoints at each epoch for
*most* fine-tuning runs, even in cases where we only report the maximal
performance. There are some exceptions: a single checkpoint for the larger
models, such as Code Llama 70b, take 100s of gigabytes of disk space. We were
not able to save all of these checkpoints with the storage that we had
available.

*Where are they available?* The fine-tuned models are available and tagged in
these repositories:

- [nuprl/MultiPL-T-StarCoderBase_1b](https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_1b)
- [nuprl/MultiPL-T-StarCoderBase_15b](https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_15b)
- [nuprl/MultiPL-T-CodeLlama_34b](https://huggingface.co/nuprl/MultiPL-T-CodeLlama_34b)
- [nuprl/MultiPL-T-StarCoder2_15B](https://huggingface.co/nuprl/MultiPL-T-StarCoder2_15B)
- [nuprl/MultiPL-T-DeepSeekCoder_33b](https://huggingface.co/nuprl/MultiPL-T-DeepSeekCoder_33b)

There are over 100 tagged models (and dozens of others) in these repositories.
You can view all the tags for a repository, e.g., for the fine-tuned
StarCoderBase-1b models as follows:

```bash
huggingface-cli tag -l nuprl/MultiPL-T-StarCoderBase_1b
```

Each tag is named `LANG-EXPERIMENT-EPOCH`. The `LANG` and `EPOCH` should
be self-explanatory. The `EXPERIMENT` is as follows:

- `morestack`: the models fine-tuned for Figure 3a
- `balancedstack`: the models fine-tuned for Figure 3b
- `25k`: the models fine-tuned for Figure 8
- `multiplt`: the primary MultiPL-T models (Tables 2 and 3). We have included
  checkpoints for several epochs, but the tables only report a result
  on the best epoch. These best epochs are listed in Table 9 in the Appendix.

It should be possible to re-evaluate any one of these fine-tuned models by
following the directions in *Evaluating a Fine-Tuned Model* above. The only
change that is in Step 2, where we specified the `--name`, `--revision`, and
`--tokenizer_name` flags.

Some caveats and suggestions if you choose to evaluate the larger models:

1. To evaluate a 15B parameter model, such as a fine-tuned version of
   StarCoderBase-15b or StarCoder2, you will need a GPU with 40GB VRAM,
   such as an A100 or A6000. (A 22GB GPU may work.)

2. To evaluate a 33B parameter model, you will need a GPU with 80GB VRAM.
    You will also need to pass the `--flash-attention2` flag to `automodel.py`
    and install [Flash Attention](https://github.com/Dao-AILab/flash-attention).

3. To evaluate a 70B parameter model, you will need 4x80GB GPUs and Flash
   Attention as described above. An alternative is to use the `automodel_vllm.py`
   script instead of `automodel.py`, which depends on [vLLM](https://docs.vllm.ai/en/latest/).

[FILL]

### The Fine-Tuning Datasets and Fine-Tuning a Model

The MultiPL-T datasets are in this repository (one split per language):

https://huggingface.co/datasets/nuprl/MultiPL-T

It is possible to fine-tune a model using these datasets instead of evaluating
a pre-trained model. 

Fine-tuning scripts tend to be optimized for particular hardware configurations
and model families. We have included a bare-bones fine-tuning script for
StarCoderBase-1b that should work on a GPU with 16GB VRAM. Here is how you
use it:

1. In the same environment that you setup in *Getting Started*, install
   [Flash Attention](https://github.com/Dao-AILab/flash-attention).

2. Within the MultiPL-T repository, enter the `training_starcoder1b` directory
   and examine `demo.py`:

   ```bash
   cd training_starcoder1b
   cat demo.py
   ```

   The hyperparamters in this script are those reported in the paper. You can
   see that it loads the Racket split, and you can use a different dataset if
   desired. *Do not use a non-StarCoder model. The training code is specialized
   for the StarCoder architecture.* 

3. Start fine-tuning.

   ```bash
   python3 demo.py
   ```

   We recommend monitoring memory usage with `nvidia-smi`. If you have more
   than 15GB VRAM, you can try to increase the `per_device_batch_size`. E.g.,
   you can set it to `4` on an 80GB GPU.

   As the code runs, it will save checkpoints at each epoch in the current
   directory. (They are named `checkpoint_N`, where `N` is the number of
   optimizer steps and not the epoch number. This is convention in LLM
   training.)

4. You can evaluate these checkpoints using the directions in *Evaluating a
   Fine-Tuned Model*. Just use the directory name as the `--name` flag. There is
   no need to specify a `--revision` or a `--tokenizer_name`.

### Fine-Tuning Larger Models

*Not recommended.*

We strongly recommend not trying to fine-tune larger models. They require
a lot more patience, more hardware, and much more complex software. This is the
software stack that we used to fine-tune the larger models:

https://github.com/cassanof/finetuning-harness/

### Generating the MultiPL-T Datasets

*Not recommended.*

We strongly recommend not trying to regenerate the MultiPL-T datasets. We
estimate it takes about 2,000 A100 GPU hours per language, assuming everything
goes perfectly. Moreover, we don't have estimate of how much CPU time is needed
to run tests, but it's probably a lot of time too.

[FILL]

### Appendix A

See the directory `./A_A_Full_Self_Instruction-Experiment` in this repository.

[A100]: https://www.nvidia.com/en-us/data-center/a100/
