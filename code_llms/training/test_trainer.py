from .trainer import train, make_evaluate
from transformers import AutoModelForCausalLM, AutoTokenizer, get_scheduler
from accelerate import Accelerator
from torch.optim import AdamW
import torch
from pathlib import Path
from torch.utils.data.dataloader import DataLoader
from .packed_strings_dataset import PackedStringsDataset
from .generate_for_evaluate import GenerateForEvaluate

TRAIN_DATA = [{ "content": "def factorial(n):\n    if n == 0:\n        return 1\n    else:\n        return n * factorial(n-2)\n" }]
TEST_DATA = [
    { "prompt": "def factorial(n):", "language": "py", "name": "python" }, 
    { "prompt": "local function factorial(n)", "language": "lua", "name": "lua" }
]

def test_train_1():
    """
    Warning: This test is flaky! We try to train a tiny StarCoder model to produce n-2 in factorial
    instead of n-1.
    """
    accelerator = Accelerator(project_dir="test_train", log_with="tensorboard")
    accelerator.init_trackers("test_train")

    # MODEL = "/home/arjun/models/starcoderbase-1b"
    MODEL = "bigcode/tiny_starcoder_py"
    model = AutoModelForCausalLM.from_pretrained(MODEL, use_cache=False, device_map= {"": accelerator.device})
    tokenizer = AutoTokenizer.from_pretrained(MODEL, padding_side="left")
    tokenizer.pad_token = tokenizer.eos_token
    # model.pad_token_id = tokenizer.pad_token_id
    # tokenizer.pad_token_id = model.eos_token_id


    NUM_EPOCHS=3

    tokenized_train_data = [ tokenizer(item["content"], return_attention_mask=False)for item in TRAIN_DATA ]

    packed_train_data = PackedStringsDataset(tokenized_train_data, 2048, tokenizer.eos_token_id, epochs=NUM_EPOCHS)

    packed_train_data.set_length_with_exact_count()

    optim = AdamW(model.parameters(), lr=0.00001)
    lr_scheduler = get_scheduler(name="constant", optimizer=optim)

    train_dataloader = DataLoader(packed_train_data, batch_size=1)
    test_dataloader = DataLoader(TEST_DATA, batch_size=2)

    train_dataloader, test_dataloader, optim, lr_scheduler, model = accelerator.prepare(
        train_dataloader, test_dataloader, optim, lr_scheduler, model
    )

    evaluate = GenerateForEvaluate.from_jsonl(model, tokenizer,
                                              Path("humaneval-py-keep.jsonl"), batch_size=2, num_return_sequences=2)

    train(
        accelerator=accelerator,
        model=model,
        evaluate=evaluate,
        train_dataloader=train_dataloader,
        max_steps=packed_train_data.set_length_with_exact_count(),
        epochs=NUM_EPOCHS,
        gradient_accumulation_steps=1,
        optim=optim,
        lr_scheduler=lr_scheduler,
    )


def test_train_0():
    """
    Warning: This test is flaky! We try to train a tiny StarCoder model to produce n-2 in factorial
    instead of n-1.
    """
    accelerator = Accelerator(project_dir="test_train", log_with="tensorboard")
    accelerator.init_trackers("test_train")

    # MODEL = "/home/arjun/models/starcoderbase-1b"
    MODEL = "bigcode/tiny_starcoder_py"
    model = AutoModelForCausalLM.from_pretrained(MODEL, use_cache=False, device_map= {"": accelerator.device})
    tokenizer = AutoTokenizer.from_pretrained(MODEL)
    # tokenizer.pad_token = tokenizer.eos_token
    # model.pad_token_id = tokenizer.pad_token_id
    # tokenizer.pad_token_id = model.eos_token_id


    NUM_EPOCHS=3

    tokenized_train_data = [ tokenizer(item["content"], return_attention_mask=False)for item in TRAIN_DATA ]
    tokenized_test_data = [ tokenizer(item["content"], return_attention_mask=False) for item in TEST_DATA ]

    packed_train_data = PackedStringsDataset(tokenized_train_data, 2048, tokenizer.eos_token_id, epochs=NUM_EPOCHS)
    packed_test_data = PackedStringsDataset(tokenized_test_data, 2048, tokenizer.eos_token_id, epochs=1)


    packed_train_data.set_length_with_exact_count()

    optim = AdamW(model.parameters(), lr=0.00001)
    lr_scheduler = get_scheduler(name="constant", optimizer=optim)

    train_dataloader = DataLoader(packed_train_data, batch_size=1)
    test_dataloader = DataLoader(packed_test_data, batch_size=1)

    train_dataloader, test_dataloader, optim, lr_scheduler, model = accelerator.prepare(
        train_dataloader, test_dataloader, optim, lr_scheduler, model
    )

    evaluate = make_evaluate(model, test_dataloader, accelerator)

    def evaluate(step):
        nonlocal model, tokenizer
        try:
            # We add \n\t\if to force the recursive solution.
            inputs = tokenizer(["def factorial(n):\n\tif"], return_tensors="pt", return_token_type_ids=False).to(accelerator.device)
            with torch.no_grad():
                output = model.generate(**inputs, max_length=40, do_sample=True, temperature=0.2, use_cache=True, pad_token_id=tokenizer.eos_token_id)
            [result] = tokenizer.batch_decode(output, clean_up_tokenization_spaces=False, skip_special_tokens=True)
            print(result)
            assert(result.startswith("def factorial(n):"))
            if step == 0:
                assert("==" in result)
                assert(("n - 1" in result) or ("n-1" in result))
            else:
                assert("n-2" in result)
        finally:
            model.train()


    train(
        accelerator=accelerator,
        model=model,
        evaluate=evaluate,
        train_dataloader=train_dataloader,
        max_steps=packed_train_data.set_length_with_exact_count(),
        epochs=NUM_EPOCHS,
        gradient_accumulation_steps=1,
        optim=optim,
        lr_scheduler=lr_scheduler,
    )


