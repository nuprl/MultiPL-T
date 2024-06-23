# Illustrating Self-Instruction

- `./examples` has the seed programs, which also appear in the Appendix.

- `./output_0.2` and `./output_0.8` has several self-instruction generations from StarCoderBase-15B at temperature 0.2 and 0.8.

- `make_examples.py` is the script we used to generate the outputs.
  
  All it does is prompt the model with the examples, and has it generate code. One could do this with a web interface to an LLM.