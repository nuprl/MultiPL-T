from transformers import AutoTokenizer, get_scheduler
from transformers import AutoModelForCausalLM
from torch.utils.data.dataloader import DataLoader
from .starcoder import starcoder_lora, starcoder_params_for_scheduler
from tqdm import tqdm
from torch.optim import AdamW
from pathlib import Path
from .packed_strings_dataset import PackedStringsDataset
from .generate_for_evaluate import GenerateForEvaluate
from .trainer import train
import datasets
from typing import Union, Optional
import torch
import sys

from .util import read_jsonl


def print_trainable_parameters(model):
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


def simple_train(
    model: int,
    batch_size: int,
    epochs: int,
    learning_rate: float,
    warmup_steps: int,
    train_data: datasets.Dataset,
    test_data: list,
    dataset_limit: int,
    log_dir: Union[str, Path],
    schedule: str = "cosine",
):
    # accelerator = Accelerator(project_dir=log_dir, log_with="tensorboard")
    per_device_batch_size = 1  # TODO(arjun): parameterize
    aggregate_device_batch_size = per_device_batch_size # supports only single gpu
    gradient_accumulation_steps = batch_size // aggregate_device_batch_size

    assert batch_size >= aggregate_device_batch_size
    assert batch_size % aggregate_device_batch_size == 0
    assert (
        len(list(Path(".").glob("checkpoint*"))) == 0
    ), "Checkpoint resume not supported"


    tokenizer = AutoTokenizer.from_pretrained(model)
    tokenizer.pad_token = tokenizer.eos_token


    if dataset_limit:
        train_data = train_data.select(range(dataset_limit))
        
    train_data.set_transform(
        lambda item: tokenizer(item["content"], return_attention_mask=False)
    )
    dataset = PackedStringsDataset(train_data, 2048, tokenizer.eos_token_id, epochs=epochs)

    # This is the number of training steps executed on each device.
    max_steps = dataset.set_length_with_exact_count() // per_device_batch_size
    max_steps = max_steps // 1 # single gpu
    
    # We load the model after the dataset so that dataset preparation errors happen immediately.
    model = AutoModelForCausalLM.from_pretrained(model, torch_dtype=torch.bfloat16, use_cache=False).cuda()
    
    optim = AdamW(
        starcoder_params_for_scheduler(model), lr=learning_rate
    )
    lr_scheduler = get_scheduler(
        name=schedule,
        optimizer=optim,
        num_warmup_steps=warmup_steps,
        num_training_steps=max_steps * 1,
    )

    train_dataloader = DataLoader(dataset, batch_size=per_device_batch_size)
    
    evaluate = GenerateForEvaluate.from_jsonl(model, tokenizer, Path(test_data),  batch_size=1, num_return_sequences=20)

    train(
        model=model,
        evaluate=evaluate,
        train_dataloader=train_dataloader,
        max_steps=max_steps,
        epochs=epochs,
        gradient_accumulation_steps=gradient_accumulation_steps,
        optim=optim,
        lr_scheduler=lr_scheduler,
    )
