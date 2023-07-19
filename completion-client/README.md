# Client for generating a single completion for each prompt 

## How does all of this work? 

There are 4 main processes (reader, completion, runner, and writer) in the
program that all communicate via 3 channels: `compl_queue`, `run_queue`, and
`fin_queue`. The *reader* process reads individual records from the JSONL prompt
file, deserializes them into our main data-structure `Program`, and adds them to
the `compl_queue`. The *completion* process concurrently reads programs from the
`compl_queue` and requests completions from the inference server (exogenous to
this program). Once it receives a completion, it sends the completed program to 
the `run_queue`. The *runner* process concurrently reads complete processes from 
the `run_queue` and runs them to check their test-cases. If the program passes 
the test-cases it is placed onto the `fin_queue`; if not, it is sent back to the 
`compl_queue` to request another completion. Finally, the *writer* process reads 
completed programs from the *fin_queue* and serializes them to disk. 
