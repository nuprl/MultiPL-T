from transformers import AutoModelForCausalLM
import pathlib
import argparse

arg_parser = argparse.ArgumentParser()
arg_parser.add_argument("--dir", type=str, required=True)
arg_parser.add_argument("--base_repo", type=str,
                        default="nuprl/MultiPLCoder-1b")
args = arg_parser.parse_args()

#  >>> m = AutoModelForCausalLM.from_pretrained("checkpoint_11796", torch_dtype=torch.bfloat16)
#  >>> m.push_to_hub("nuprl/MultiPLCoder-1b", private=True, commit_message="lua-25k-epoch6")
#  >>> m = AutoModelForCausalLM.from_pretrained("checkpoint_final", torch_dtype=torch.bfloat16)
#  >>> m.push_to_hub("nuprl/MultiPLCoder-1b", private=True, commit_message="lua-25k-epoch7")

# find all checkpoints in dir
checkpoints = []
for path in pathlib.Path(args.dir).rglob("checkpoint_*"):
    checkpoints.append(path.name)

print("Found checkpoints:")
print(checkpoints)
