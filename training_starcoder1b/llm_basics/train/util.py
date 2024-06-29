import json
from pathlib import Path
from typing import Generator, Union, List
import logging


def disable_other_loggers(level: int = logging.INFO):
    """
    Disables logging in all modules with the exception of the those in the
    parent module.
    """
    logging.basicConfig(level=logging.INFO)

    # Get the name of the parent module
    module_name_prefix = ".".join(__name__.split(".")[:-1])
    print("Disabling logging for modules not in", module_name_prefix)

    for alogger in logging.Logger.manager.loggerDict.values():
        if isinstance(alogger, logging.PlaceHolder):
            continue
        if not alogger.name.startswith(module_name_prefix):
            alogger.disabled = True


def read_jsonl(path: Union[str, Path]) -> Generator[dict, None, None]:
    with open(path) as f:
        for line in f:
            yield json.loads(line)


def trainable_parameters(model):
    """
    Prints the number of trainable parameters in the model.
    """
    trainable_params = 0
    all_param = 0
    for _, param in model.named_parameters():
        all_param += param.numel()
        if param.requires_grad:
            trainable_params += param.numel()
    print(
        f"trainable params: {trainable_params} || all params: {all_param} || trainable%: {100 * trainable_params / all_param}"
    )


def eval_schedule(max_steps: int, epochs: int, evals_per_epoch: int) -> List[int]:
    """
    Returns a schedule of steps at which to evaluate the model. The steps are
    returned in reverse order. So, check result[-1] to see if the model should
    evaluate, and .pop() it when done.
    """
    steps = []
    steps_per_epoch = max_steps // epochs
    steps_between_evals = steps_per_epoch // evals_per_epoch
    num_evals = evals_per_epoch * epochs
    for i in range(num_evals):
        steps.append((i + 1) * steps_between_evals)
    last_eval_step = steps[-1]
    last_step = max_steps - 1
    if last_step - last_eval_step < steps_between_evals:
        # If the penultimate evaluation is too close to the final step, we skip.
        steps.pop()
    steps.reverse()
    return steps


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


def save_model_and_tokenizer(
    model, tokenizer, path: Path, save_model_kwargs: dict = {}
):
    """
    Saves the model and tokenizer to path. Configures the model to use KV
    caching by default, which is what you want for inference. This does *not*
    save the optimizer state, so is not suitable for resuming training.
    """
    model.save_pretrained(path, **save_model_kwargs)
    tokenizer.save_pretrained(path)
    model_config_path = path / "config.json"
    model_config = json.loads(model_config_path.read_text())
    model_config["use_cache"] = True
    model_config_path.write_text(json.dumps(model_config, indent=2))
