"""
Some helpers functions for StarCoder models.
"""
from transformers import AutoModelForCausalLM
# from accelerate import Accelerator

def starcoder_params_for_scheduler(model, weight_decay=0.001):
    """
    We use this to configure weight decay for StarCoder models.
    """
    params_with_wd = []
    params_without_wd = []
    for n, p in model.named_parameters():
        if "bias" in n:
            params_without_wd.append(p)
        elif "ln_" in n and "weight" in n:
            params_without_wd.append(p)
        else:
            params_with_wd.append(p)
    return [
        {"params": params_with_wd, "weight_decay": weight_decay},
        {"params": params_without_wd, "weight_decay": 0.0},
    ]


def starcoder_lora(model_path: str, lora_r, lora_alpha, lora_dropout):
    """
    Loads a StarCoder model for LoRA training.
    """

    from peft import LoraConfig, get_peft_model, prepare_model_for_int8_training

    lora_config = LoraConfig(
        r=lora_r,
        lora_alpha=lora_alpha,
        lora_dropout=lora_dropout,
        bias="none",
        task_type="CAUSAL_LM",
        target_modules=["c_proj", "c_attn", "q_attn"],
    )
    model = AutoModelForCausalLM.from_pretrained(
        model_path,
        use_cache=False,
        load_in_8bit=True
    ).cuda()
    model = prepare_model_for_int8_training(model)
    model = get_peft_model(model, lora_config)
    return model
