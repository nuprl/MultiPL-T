from transformers import AutoModelForCausalLM, AutoTokenizer
import torch
import pathlib
import argparse

arg_parser = argparse.ArgumentParser()
arg_parser.add_argument("--dir", type=str, required=True)
arg_parser.add_argument("--tokenizer", type=str,
                        default="", help="Optional, tokenizer to push to hub")
arg_parser.add_argument("--base_repo", type=str,
                        default="nuprl/MultiPLCoder-1b")
arg_parser.add_argument("--checkpoint_sep", type=str,
                        default="_", help="separator between checkpoint name and epoch number")
args = arg_parser.parse_args()

#  >>> m = AutoModelForCausalLM.from_pretrained("checkpoint_11796", torch_dtype=torch.bfloat16)
#  >>> m.push_to_hub("nuprl/MultiPLCoder-1b", private=True, commit_message="lua-25k-epoch6")
#  >>> m = AutoModelForCausalLM.from_pretrained("checkpoint_final", torch_dtype=torch.bfloat16)
#  >>> m.push_to_hub("nuprl/MultiPLCoder-1b", private=True, commit_message="lua-25k-epoch7")

# find all checkpoints in dir
checkpoints = []
for path in pathlib.Path(args.dir).rglob(f"checkpoint{args.checkpoint_sep}*"):
    checkpoints.append(path.name)

dir_name = pathlib.Path(args.dir).name

if args.tokenizer:
    tok = AutoTokenizer.from_pretrained(args.tokenizer)
    tok.push_to_hub(args.base_repo, private=True)

checkpoints.sort(key=lambda x: int(x.split(args.checkpoint_sep)[1]))
for epoch, checkpoint in enumerate(checkpoints):
    epoch += 1  # 1-indexed
    commit = f"{dir_name}-epoch{epoch}"
    print(
        f"Pushing {checkpoint} (epoch {epoch}) to {args.base_repo} - {commit}")
    m = AutoModelForCausalLM.from_pretrained(
        dir_name + "/" + checkpoint,
        torch_dtype=torch.bfloat16
    )
    m.push_to_hub(args.base_repo, private=True,
                  commit_message=commit)
