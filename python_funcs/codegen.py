from transformers import AutoTokenizer, AutoModelForCausalLM
import torch


class CodeGen:
    def code_complete(self, prompts, temp):
        raise NotImplementedError


class GPTCodeGen(CodeGen):
    def __init__(self, model):
        self.model = model

    def code_complete(self, prompts, temp=0.75):
        import os
        import openai
        openai.api_key = os.getenv("OPENAI_API_KEY")
        assert openai.api_key is not None, "OpenAI API key not found"
        prompt = prompts[0]
        num_comps = len(prompts)
        # all prompts should be the same
        assert all(
            p == prompt for p in prompts), "Prompts must be the same with OpenAI"

        system = "You are a Python test-writing code assistant. You are given a function implementation, and you must write a large (10 or more) and comprehensive test suite for it. Continue generating tests from the given prompt, do not write any extra text. You must strictly write tests in the `assert func(args...) == ...` format. Do not write comments or failure messages."
        try:
            completion = openai.ChatCompletion.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": system},
                    {"role": "user", "content": prompt},
                ],
                temperature=temp,
                max_tokens=512,
                n=num_comps,
            )

            content = [c.message["content"] for c in completion.choices]
        except Exception as e:
            content = [str(e)] * num_comps
        print(f"GPT completions: {content}")
        return [prompt + c for c in content]


class HFCodeGen(CodeGen):
    def __init__(self, model, accelerator,  seq_len, load_in_8bit=False):
        model_kwargs = {}
        if load_in_8bit:
            from transformers import BitsAndBytesConfig
            quantization_config = BitsAndBytesConfig(load_in_8bit=True,
                                                     llm_int8_threshold=0.0)
            model_kwargs = {
                "quantization_config": quantization_config,
            }

        self.model = AutoModelForCausalLM.from_pretrained(
            model,
            device_map=accelerator.device,
            torch_dtype=torch.float16,
            **model_kwargs
        )
        self.tokenizer = AutoTokenizer.from_pretrained(
            model,
            padding_side="left"
        )
        self.tokenizer.pad_token = self.tokenizer.eos_token
        assert seq_len <= self.model.config.max_position_embeddings, "Sequence length too long for model"
        self.max_tokens = seq_len
        self.max_new_tokens = 400
        self.accelerator = accelerator

    def code_complete(self, prompts, temp=0.2):
        with torch.no_grad():
            tokens = self.tokenizer(
                prompts,
                return_tensors="pt",
                padding=True,
                truncation=True,
                max_length=self.max_tokens,
            ).to(self.accelerator.device)
            outputs = self.model.generate(
                **tokens,
                max_new_tokens=self.max_new_tokens,
                do_sample=True,
                top_p=0.95,
                temperature=temp,
                use_cache=True,
                pad_token_id=self.tokenizer.eos_token_id
            )
            return self.tokenizer.batch_decode(outputs, skip_special_tokens=True)
