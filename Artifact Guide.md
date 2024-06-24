# Artifact Guide

This paper is not amenable to push-button reproduction. An attempt to reproduce everything would require over 50TB of disk space and over 500 days of GPU time (on an datacenter-grade, A100 GPU). Instead, it is better to pick a small subset of data points to reproduce. If you reproduce a fine-tuned model, you will probably want to immediately evaluate it and delete the model after the reproduction.



## Figure 3a

**Fine-tuning on the complete language specific subsets of The Stack.**

All of these fine-tuned models are available at https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_1b. We can load them in Transformers with the commit hashes indicated below.

- Lua Epoch 1: 5a34651bc5fa9d23a97f2532a505299b21615ab1
- Lua Epoch 2: c08ba56ba279fd3aa2a2f83569eaa7c718b8b17a
- Lua Epoch 3: 33022a68fa634cd135fd05cf9642fba3f6a9434f
- OCaml Epoch 1: ec398488f2da879bb9d3ac8444c0b8bba99ad7c1
- OCaml Epoch 2: 602ec4d00319a05afc5963c40cd87282d3b8448b
- OCaml Epoch 3: 392291039678a66de63a5db6182843ce7d5c3675
- Racket Epoch 1: 75b3d6d8df43b9e9fa3623e3e3f5a2280b20ec94
- Racket Epoch 2: b66a6b6b09e9c246ebbe6f43a5c941659803a4df
- Racket Epoch 3: f72693d82798326e8e14b3719409d3f2cf6128b6

## Figure 3b

[FILL]

## Figure 8

**We fine-tune three versions of StarCoderBase-1B on 25k MultiPL-T generated training items.**

All of these fine-tuned models are available as revisions of [nuprl/MultiPL-T-StarCoderBase_1b](https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_1b).

- Lua Epoch 1: 3bcfd792f683a965047545bbba09c8fae5064d07
- Lua Epoch 2: 5656bb80775e618e05575057699805e64a3875cb
- Lua Epoch 3: 7e18e064e05642063abb53aad0ea0ef115eb2d8e
- Lua Epoch 4: 883f8f94bc88e15727113330e8627817a8025c42
- Lua Epoch 5: 77f92b21f9b9e5c8f773b851639fa9f9714f6da9
- Lua Epoch 6: 39be1a2cd81cee4fa3ed0f82b368ef4aff395d48
- Lua Epoch 7: 96aa7ad9f620c5150a33cddc540e0709cd81e2da
- Racket Epoch 1: 8b0399e0a561b71c7609d840349f6b344072dfea
- Racket Epoch 2: fb7825bf7f54c776c534ca3374fe7521df497ea6
- Racket Epoch 3: de40ca87e3cc5a0aea51b27eaedff44833761da4
- Racket Epoch 4: c9c02e887da23bd52932624a8dc0288603b35a16
- Racket Epoch 5: f57df9105d72b71f495c93a96b8ae0346dc0ef5a
- Racket Epoch 6: f78c1711e2fdab83ad9c664964ccfca133080f0f
- Racket Epoch 7: fba8015c3882ca88f45f1ed36c375c382b42e811
- OCaml Epoch 1: e90c2a9b47ffa5fc20a441bb9c00ce9f7f1b9699
- OCaml Epoch 2: edb0a095f92bafcb72d1f8885c4a7f0e5481841b
- OCaml Epoch 3: d92bf648d1fb58451d1b2d6616a6fa37e7f2fec4
- OCaml Epoch 4: a5c29b8490c7cf3a03cf66af52c304f0b813dfb6
- OCaml Epoch 5: 8caa08e562e12cd1f77f671e3f93afcc2a298a2b
- OCaml Epoch 6: 9d772495e962f8b0d4610c890f992f5c4b0ed975
- OCaml Epoch 7: d59c7eb53c618234df8dab194226441b27b5208c

## Table 2

**MultiPL-E pass@1 scores for 1B, 15B, 34B, and 70B parameter models before and after fine-tuned on MultiPL-T data.**

The base models are third-party models. The fine-tuned models are available in the repositories below.

- [nuprl/MultiPL-T-StarCoderBase_1b](https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_1b)
- [nuprl/MultiPL-T-StarCoderBase_15b](https://huggingface.co/nuprl/MultiPL-T-StarCoderBase_15b)
- [nuprl/MultiPL-T-CodeLlama_34b](https://huggingface.co/nuprl/MultiPL-T-CodeLlama_34b)
- [nuprl/MultiPL-T-CodeLlama_70b](https://huggingface.co/nuprl/MultiPL-T-CodeLlama_70b)

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
