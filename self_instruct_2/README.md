The goal of this exercise is to build a  self-instruction dataset and
actually train a model.

We use the CodeParrot self-instruction recipe, which was optimized forS
StarCoderBase:

https://huggingface.co/datasets/codeparrot/self-instruct-starcoder

It is a variation of the CodeAlpaca recipe. Highlights are:

1. 4 examples in context (2 generated, 2 seed)
2. 80 seed tasks
3. 5,000 generated instructed
4. Using 0.7 ROUGE-L as the deduplication threshold.
5. Temperature 0.8 and top-p 0.95 for generation
   
The CodeAlpaca technique batches generations to lower their OpenAI bill.
We don't need to bother with that since we are self-hosting.

Approach:1

1. Use build_seed_dataset_from_multipl_humaneval_rkt.ipynb to build the
   seed dataset.