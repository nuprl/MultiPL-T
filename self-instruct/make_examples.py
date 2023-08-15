from argparse import ArgumentParser
from transformers import AutoModelForCausalLM, AutoTokenizer
from pathlib import Path
import torch

MODEL = "/home/arjun/models/starcoderbase"
MAX_TOKENS = 1024
DIVIDER = "\n\n" + "-" * 20 + "\n\n"
STOP_TOKENS = ["\n(define ", "\n#|", "\n;","\n(", "#lang", DIVIDER]
TOP_P = 0.95

def collect_context():
    context = ""
    for p in Path("./examples").glob("*.rkt"):
        with open(p) as f:
            func = f.read()
            func = func[:func.find("<tests>")].rsplit("\n", 1)[0]
        context += func
        context += DIVIDER
    return context

class Model:
    def __init__(self, name, revision=None, tokenizer_name=None, tokenizer_revision=None):
        self.model = AutoModelForCausalLM.from_pretrained(name, revision=revision, torch_dtype=torch.bfloat16, trust_remote_code=True).cuda()
        self.tokenizer = AutoTokenizer.from_pretrained(tokenizer_name or name, revision=tokenizer_revision or revision, padding_side="left", trust_remote_code=True)
        self.tokenizer.pad_token = "<|endoftext|>"
        
    def completion_tensors(
        self,
        prompts: list,
        max_length: int,
        temperature: float,
        top_p: float,
    ):
        inputs = self.tokenizer(prompts, padding=True, return_tensors="pt", return_token_type_ids=False).to("cuda")
        with torch.no_grad():
            output = self.model.generate(
                **inputs,
                do_sample=True,
                top_p=top_p,
                temperature=temperature,
                max_length=max_length,
                pad_token_id=self.tokenizer.pad_token_id
            )
        return output

    def decode_single_output(self, output_tensor, prompt):
        # NOTE(arjun): skip_special_tokens=True is the convenient way to strip out the left-side
        # padding tokens.
        detok_hypo_str = self.tokenizer.decode(
            output_tensor, clean_up_tokenization_spaces=False, skip_special_tokens=True,
        )
        # Skip the prompt (which may even have stop_tokens)
        return detok_hypo_str[len(prompt) :]

    def completions(
        self, prompts: str, max_tokens: int, temperature: float, top_p, stop
    ):
        prompts = [ prompt.strip() for prompt in prompts ]
        output_tensors = self.completion_tensors(
            prompts,
            max_tokens,
            temperature,
            top_p,
        )
        return [
            stop_at_stop_token(self.decode_single_output(output_tensor, prompt), stop + ["<|endoftext|>"])
            for (prompt, output_tensor) in zip(prompts, output_tensors)
        ]

def stop_at_stop_token(decoded_string, stop_tokens):
    """
    Produces the prefix of decoded_string that ends at the first occurrence of
    a stop_token.

    WARNING: the decoded_string *must not* include the prompt, which may have stop tokens
    itself.
    """
    min_stop_index = len(decoded_string)
    for stop_token in stop_tokens:
        stop_index = decoded_string.find(stop_token)
        if stop_index != -1 and stop_index < min_stop_index:
            min_stop_index = stop_index
    return decoded_string[:min_stop_index]

def generate_functions(model, context, batch_size, temperature):
    prefixes = [";;"] * batch_size

    signatures = model.completions(
        [context + prefix for prefix in prefixes],
        max_tokens=MAX_TOKENS, 
        temperature=temperature, 
        top_p=TOP_P, 
        stop=["\n", "\n(define"]
    )

    prefixes = [prefix + signature + "\n;;" for prefix, signature in zip(prefixes, signatures)]

    descriptions = model.completions(
        [context + prefix for prefix in prefixes],
        max_tokens=MAX_TOKENS, 
        temperature=temperature, 
        top_p=TOP_P, 
        stop=["\n(define"]
    )

    prefixes = [prefix + description + "\n(define (" for prefix, description in zip(prefixes, descriptions)]

    functions = model.completions(
        [context + prefix for prefix in prefixes],
        max_tokens=MAX_TOKENS, 
        temperature=temperature, 
        top_p=TOP_P, 
        stop=STOP_TOKENS
    )

    return [prefix + func for prefix, func in zip(prefixes, functions)]
    

def main():
    args = ArgumentParser()
    args.add_argument("num_problems", type=int)
    args.add_argument("--temperature", type=float, default=0.8)
    args.add_argument("--batch_size", type=int, default=25)
    args.add_argument("--output_dir", type=Path, default=Path("./output"))
    args = args.parse_args()

    args.output_dir.mkdir(parents=True, exist_ok=True)

    model = Model(MODEL)

    context = collect_context()

    functions = []

    while len(functions) < args.num_problems:
        functions.extend(generate_functions(model, context, args.batch_size, args.temperature))

    for i, func in enumerate(functions):
        with open(args.output_dir.joinpath(f"example_{i}.rkt"), "wt+") as f:
            f.write(func)

if __name__ == "__main__":
    main()