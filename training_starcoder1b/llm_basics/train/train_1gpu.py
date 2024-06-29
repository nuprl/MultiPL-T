from transformers import AutoTokenizer, get_scheduler
from transformers import AutoModelForCausalLM
from torch.utils.data.dataloader import DataLoader
from .starcoder import starcoder_params_for_scheduler
from torch.optim import AdamW
from pathlib import Path
from .packed_strings_dataset import PackedStringsDataset
from torch.utils.tensorboard import SummaryWriter
import datasets
import torch.nn.utils
from typing import Union, Literal
import torch
from tqdm import tqdm
from .util import eval_schedule, save_model_and_tokenizer
import json
import numpy as np


def evaluate(model, writer, eval_dataloader: DataLoader, optimizer_step):
    # Put model in eval mode
    model.eval()

    global_mean_loss = []

    with torch.no_grad():
        for inputs in tqdm(eval_dataloader, desc="Evaluating"):
            outputs = model(**inputs, use_cache=False)
            loss = outputs["loss"] if isinstance(outputs, dict) else outputs[0]
            global_mean_loss.append(loss.mean().item())

    loss = sum(global_mean_loss) / len(global_mean_loss)
    writer.add_scalar("eval/loss", loss, optimizer_step)
    model.train()


def train(
    model: str,
    global_batch_size: int,
    per_device_batch_size: int,
    epochs: int,
    learning_rate: float,
    warmup_steps: int,
    train_data: datasets.Dataset,
    eval_data: datasets.Dataset,
    step_counter: Union[Literal["exact"], Literal["approximate"]],
    max_seq_length: int,
    schedule: str = "cosine",
    skip_last_batch: bool = False,
    use_flash_attention_2: bool = False,
    model_kwargs: dict = {},
    tokenizer_kwargs: dict = {},
):
    """
    Trains a model on a dataset.

    Args:
        model: The model to train. This can be a path to a local directory or a model name.
        global_batch_size: The total batch size across all GPUs.
        per_device_batch_size: The batch size per GPU.
        epochs: The number of epochs to train for.
        learning_rate: The learning rate.
        warmup_steps: The number of warmup steps.
        train_data: The training data.
        eval_data: The evaluation data.
        step_counter: How to count steps. If "approximate", the number of steps is
            calculated by dividing the length of the dataset by the batch size.
            If "exact", the number of steps is calculated by iterating over the
            dataset once and counting the number of batches.
        schedule: The learning rate schedule.
        skip_last_batch: Whether to skip the last batch. This is useful when the
            last batch is smaller than the batch size.
        use_flash_attention_2: Whether to use FlashAttention2.
    """

    # It is a little silly that gradient_accumulation_steps isn't an argument
    # to the train function. But, I find it easier to have the batch size
    # as an explicit argument (global_batch_size).
    gradient_accumulation_steps = global_batch_size // per_device_batch_size

    assert (
        gradient_accumulation_steps >= 1
    ), f"Calculated gradient_accumulation_steps: {gradient_accumulation_steps} is less than 1."
    assert (
        global_batch_size >= per_device_batch_size
    ), f"Calculated global_batch_size: {global_batch_size} is less than per_device_batch_size: {per_device_batch_size}"
    assert (
        global_batch_size % per_device_batch_size == 0
    ), f"Calculated global_batch_size: {global_batch_size} is not divisible by per_device_batch_size: {per_device_batch_size}"

    existing_checkpoints = list(Path(".").glob("checkpoint*"))
    assert (
        len(existing_checkpoints) == 0
    ), f"Checkpoint resume not supported. Move or delete {existing_checkpoints}."

    if use_flash_attention_2:
        assert (
            skip_last_batch
        ), "skip_last_batch must be True when using FlashAttention2."

    tokenizer = AutoTokenizer.from_pretrained(model, **tokenizer_kwargs)
    tokenizer.pad_token = tokenizer.eos_token

    # We do not need the attention mask from the tokenizer because
    # PackedStringsDataset will build it.
    train_data.set_transform(
        lambda item: tokenizer(item["content"], return_attention_mask=False)
    )

    dataset = PackedStringsDataset(
        train_data,
        max_seq_length,
        tokenizer.eos_token_id,
        epochs=epochs,
        skip_last_batch=skip_last_batch,
        device="cuda",
    )

    # See above for why return_attention_mask=False.
    eval_data.set_transform(
        lambda item: tokenizer(item["content"], return_attention_mask=False)
    )
    eval_dataset = PackedStringsDataset(
        eval_data,
        max_seq_length,
        tokenizer.eos_token_id,
        epochs=1,
        skip_last_batch=skip_last_batch,
        device="cuda",
    )

    eval_dataset.set_length_with_exact_count()

    # This is the number of training steps executed on each GPU.
    if step_counter == "approximate":
        max_steps = dataset.set_length_with_approximate_count() // per_device_batch_size
    else:
        print("Calculating exact dataset length. This can take some time...")
        max_steps = dataset.set_length_with_exact_count() // per_device_batch_size

    print("Loading model. This can take some time...")

    # We load the model after the dataset so that dataset preparation errors happen immediately.
    model = AutoModelForCausalLM.from_pretrained(
        model,
        torch_dtype=torch.bfloat16,
        use_flash_attention_2=use_flash_attention_2,
        use_cache=False,
        **model_kwargs,
    ).cuda()

    optim = AdamW(starcoder_params_for_scheduler(model), lr=learning_rate)

    # The optimizer will not step max_steps times. Since we are likely using
    # gradient accumualation, we divide by gradient_accumulation_steps to get
    # the number of optimizer steps.
    lr_scheduler = get_scheduler(
        name=schedule,
        optimizer=optim,
        num_warmup_steps=warmup_steps,
        num_training_steps=max_steps // gradient_accumulation_steps,
    )

    train_dataloader = DataLoader(dataset, batch_size=per_device_batch_size)
    eval_dataloader = DataLoader(eval_dataset, batch_size=2)

    writer = SummaryWriter()

    evaluate(model, writer, eval_dataloader, optimizer_step=0)

    eval_steps = eval_schedule(max_steps, epochs, 1)

    optimizer_step = 0
    current_losses = [ ]
    for step, inputs in tqdm(
        enumerate(train_dataloader),
        total=max_steps,
        unit="batch",
        desc="Training",
    ):
        if step >= max_steps:
            break

        outputs = model(**inputs, use_cache=False)
        loss = outputs.loss / gradient_accumulation_steps
        # Do not step the optimizer. The gradients thus accumulate (i.e., get
        # added up) and model parameters do not update.
        loss.backward()
        current_losses.append(loss.mean().item())
        
        if len(eval_steps) > 0 and step == eval_steps[-1]:
            eval_steps.pop()
            checkpoint_dir = Path(f"checkpoint_{optimizer_step}")
            save_model_and_tokenizer(model, tokenizer, checkpoint_dir)
            evaluate(model, writer, eval_dataloader, optimizer_step=optimizer_step)

        # Gradient accumulation
        if step % gradient_accumulation_steps != 0:
            continue

        # Log metrics to both stderr and TensorBoard.
        metrics = {
            "train/loss": np.mean(current_losses),
            "train/lr": optim.param_groups[0]["lr"],
        }
        tqdm.write(json.dumps(metrics))
        for k, v in metrics.items():
            writer.add_scalar(k, v, optimizer_step)

        current_losses.clear()
        optimizer_step += 1

        torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)

        # Update model weights with the accumulated gradients.
        optim.step()
        # Adjust learning rate by the schedule.
        lr_scheduler.step()
        # Clear accumulated gradients.
        optim.zero_grad()

    final_checkpoint_dir = Path(f"checkpoint_{step}")
    save_model_and_tokenizer(model, tokenizer, final_checkpoint_dir)
    Path("checkpoint_final").symlink_to(final_checkpoint_dir)
    evaluate(model, writer, eval_dataloader, optimizer_step=optimizer_step)
