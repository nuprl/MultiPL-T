from argparse import ArgumentParser
from transformers import AutoModelForCausalLM, AutoTokenizer
from pathlib import Path

MODEL = "bigcode/starcoderbase"
MAX_TOKENS = 1024
STOP_TOKENS = ["\n(define ", "\n#|", "\n;","\n("]
TEMPERATURE = 0.8

def collect_context():
    context = ""
    for p in Path("./examples").glob("*.rkt"):
        with open(p) as f:
            func = f.read()
            func = func[:func.find("<tests>")].rsplit("\n", 1)[0]
        context += func
        context += "\n\n" + "-" * 20 + "\n\n"
    return context


def main():
    args = ArgumentParser()
    args.add_argument("num_problems", type=int)
    args.add_argument("--batch_size", type=int, default=10)
    args.add_argument("--out_dir", type=Path, default=Path("output"))
    args = args.parse_args()

    model = AutoModelForCausalLM.from_pretrained(MODEL, low_cpu_mem_usage=True, trust_remote_code=True).eval().cuda()
    tokenizer = AutoTokenizer.from_pretrained(MODEL, padding_side='left', clean_up_tokenization_spaces=False)

    context = collect_context()

    inputs = tokenizer.tokenize(context + "(define (")

    outputs = model.generate(**inputs, max_new_tokens=MAX_TOKENS,
                                         pad_token_id=tokenizer.eos_token_id,
                                         temperature=TEMPERATURE,
                                         do_sample=True)
    
    gentext = tokenizer.decode(outputs)
    print(gentext[0])

if __name__ == "__main__":
    main()