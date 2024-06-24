# Appendix A

The goal of this exercise is to build a self-instruction dataset and
actually train a model.

We use the CodeParrot self-instruction recipe, which was optimized for
StarCoderBase:

https://huggingface.co/datasets/codeparrot/self-instruct-starcoder

It is a variation of the CodeAlpaca recipe. Highlights are:

1. 4 examples in context (2 generated, 2 seed)
2. 78 seed tasks
3. 5,000 generated instructed
4. Using 0.7 ROUGE-L as the deduplication threshold.
5. Temperature 0.8 and top-p 0.95 for generation
   
The CodeAlpaca technique batches generations to lower their OpenAI bill.
We don't need to bother with that since we are self-hosting.

Approach:

1. Use `build_seed_dataset.ipynb` to build the
   seed dataset. Stored in `seeds.jsonl`.
2. Run `generator.py` to generate the self instruction dataset. Data stored at:
   - https://huggingface.co/datasets/nuprl/MultiPL-T-racket-selfinstruct
   - On NU Discovery Cluster at 
3. Fine-tune and evaluate as described in the paper. Models are available here:
   - https://huggingface.co/nuprl/MultiPL-T-starcoderbase1b-racket-selfinstruct