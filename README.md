# MultiPL-T
Generating data for low-resource languages from Python, Javascript, and Typescript. 

## Content Table
[Federico TODO] This is temporary, write a good README.md. i wrote this so that people don't get lost in the code.
- `./benchmarks/` here is the code for ClassBench and pass_matrix
- `./completion-client` the Rust completion client
- `./source_lang_processing` the code for generating the source Python dataset, running typechecks, and test generation
- `./multipl_e_target_adaptor` adaptor code for using MultiPL-E as the target completion engine
- `./target_lang_postprocessing` code for post-processing the generated target code. this includes dedup stuff, edu scoring, etc..
