# MultiPL-T Completion Engine 

This engine connects a custom client to the :huggingface:
text-generation-inference server that requests each prompt for a set number of 
times, and evaluates completions to generate new MultiPL-T training items. 

## Prompt schema

```json
{
    "name": string, //name of the problem 
    "language": string, //language of the problem
    "prompt": string, //the actual prompt
    "original": string, //the path to file that generated the prompt 
    "tests": string, //the test suite for the prompt
    "stop_tokens": array<string> //stop sequences for generation
}
```

## Requirements 
 
### Python 

- Python >= 3.8
- chevron=0.14.0 

### System requirements 

- 80gb VRAM NVIDIA GPU (A100, H100)
- Podman
- GNU-make
- `multipl-e-evaluation` container built from `dev` branch 

## Generating completions 

At the top of the `Makefile` you can set the paths to the various resources
needed to run the completion engine. Then, running `make` should start the
completion engine by building a Kubernetes config and then having Podman play
it. Running `make build` builds the container that runs the client portion of
the engine. Running `make setup` generates the Kubernetes YAML necessary to 
run the engine. 

