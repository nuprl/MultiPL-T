# MultiPL-E Adapter for Generating the MultiPL-T Datasets

MultiPL-E's evaluator contains several code generation optimizations that can be exploited for
synthetic dataset generation purposes, hence why we built an adapter to utilize it in this work.

To utilize the adapter, your directory structure should look like this:

```
<root>/MultiPL-T
<root>/MultiPL-E
```

Where `<root>` is the parent directory of this repository.

### Running the Adapter

To run the adapter, the `gen_prompts.sh` script should be used. The script takes in the following
arguments:

```
./gen_prompts.sh <language> <root> <output_dir> <num gpus>
```

Where `<language>` is the language of the generated code, `<root>` is the parent directory of this
repository, `<output_dir>` is the directory where the generated prompts will be stored, and `<num gpus>`
is the number of GPUs to use for the generation process.

We support all languages in MultiPL-E for this step.
