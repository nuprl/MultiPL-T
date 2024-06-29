# Benchmarks
These benchmarks were created as a way to differentiate between the HumanEval style benchmarks that are normally used in testing model performance. These benchmarks were created to be utilized with MultiPL-E to ensure ease of execuation.

There are two sets of benchmarks that were created: **ClassBench** and **CryptoBench**. **ClassBench** defines function and data types in context which are necessary to fulfill the given prompt. **CryptoBench** has problems which requires the use of cryptography libraries in context to complete the given prompt.

## Execution
If you wished to run these benchmarks you would have to run in the MultiPL-E repository `python automodel.py --use-local --dataset /path/to/MultiPL-T/benchmarks/{benchmark name}/{language}-prompts/multiple_form_prompts.json {other options}`.

