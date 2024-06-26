"""
This file contains a trainer that is a bit less sophisticated than the Hugging Face trainer, thus
easier to understand.
"""
from transformers import AutoModelForCausalLM
import torch
from pathlib import Path
from tqdm import tqdm
from collections.abc import Mapping
import json
import sys

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def save_checkpoint(model, path):
    # NOTE(arjun): This does not save the optimizer state, so it is not suitable
    # for resuming from a crashed run.
    model.save_pretrained(path)

def is_approx_end_of_epoch(max_steps: int, epochs: int, current_step: int) -> bool:
    """
    Returns whether the current step is approximately the end of an epoch.
    """
    if current_step == 0:
        return False
    if current_step == max_steps:
        return True
    if max_steps // epochs == 0:
        # We are running for less than a complete epoch.
        return current_step == max_steps
    return current_step % (max_steps // epochs) == 0


def train(model, evaluate, train_dataloader, gradient_accumulation_steps, epochs, max_steps, optim, lr_scheduler):
    model.train()
    evaluate(checkpoint_dir=None, step=0)

    for step, batch in tqdm(
        enumerate(train_dataloader),
        total=max_steps,
        unit="batch",
        desc="Training",
    ):
        if step >= max_steps:
            break
        
        ## (fran) copied from hf Trainer
        def _prepare_input(data):
            """
            Prepares one `data` before feeding it to the model, be it a tensor or a nested list/dictionary of tensors.
            """
            if isinstance(data, Mapping):
                return type(data)({k: _prepare_input(v) for k, v in data.items()})
            elif isinstance(data, (tuple, list)):
                return type(data)(_prepare_input(v) for v in data)
            elif isinstance(data, torch.Tensor):
                kwargs = {"device": model.device}
                return data.to(**kwargs)
            return data
        
        inputs = _prepare_input(batch)
        outputs = model(**inputs, use_cache=False)
        loss = outputs["loss"] if isinstance(outputs, dict) else outputs[0]
        loss.backward()
        global_mean_loss = loss.mean().item()
        
        # Gradient accumulation: we only step the optimizer every N steps.
        if step % gradient_accumulation_steps == 0:
            optim.step()
            lr_scheduler.step()
            optim.zero_grad()

        if is_approx_end_of_epoch(max_steps, epochs, step):
            checkpoint_dir = Path(f"checkpoint_{step}")
            save_checkpoint(model, checkpoint_dir)
            evaluate(checkpoint_dir, step)

        # Log metrics to both stderr and TensorBoard.
        metrics = {
            "train/loss": global_mean_loss,
            "train/lr": optim.param_groups[0]["lr"],
            "train/loss": global_mean_loss,
        }
        eprint(json.dumps(metrics))

        
    checkpoint_dir = Path(f"checkpoint_final")
    evaluate(checkpoint_dir, step)
    save_checkpoint(model, checkpoint_dir)

