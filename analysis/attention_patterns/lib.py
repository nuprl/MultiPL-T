import torch
import pandas as pd
from termcolor import colored

FORE="black"
def color_tok(attn, tok, distr) -> str:
    if attn < distr["25%"]:
        return colored(tok, color=FORE, on_color='on_light_grey')
    elif attn < distr["50%"]:
        return colored(tok, color=FORE, on_color='on_green')
    elif attn < distr["75%"]:
        return colored(tok, color=FORE, on_color='on_yellow')
    else:
        return colored(tok, color=FORE, on_color='on_red')
    

def visualize_attn(tokenizer, out, meaned_attns):
    distr = pd.DataFrame(meaned_attns.detach().cpu().numpy()).describe()[0]
    buf = ""
    for i, tok in enumerate(out):
        colored = color_tok(meaned_attns[i], tokenizer.decode(tok), distr)
        buf += colored
    return buf

def attn_distribution(tokenizer, out, meaned_attns):
    attns_in_comment = []
    attns_in_sig = []
    attns_in_body = []
    
    in_comment = True
    in_sig = False
    in_body = False

    prev = None
    for i, tok in enumerate(out):
        if prev is None:
            prev_dec = ""
        else:
            prev_dec = tokenizer.decode(prev)
        dec = tokenizer.decode(tok)
        prev = tok
        attn = meaned_attns[i]
        
        if "local" in dec and "\n" in prev_dec and in_comment:
            in_sig = True
            in_comment = False
        elif ")" in prev_dec and in_sig:
            in_body = True
            in_sig = False

        if in_comment:
            attns_in_comment += [attn]
        elif in_sig:
            attns_in_sig += [attn]
        elif in_body:
            attns_in_body += [attn]
    
    attns_in_comment = torch.tensor(attns_in_comment)
    attns_in_sig = torch.tensor(attns_in_sig)
    attns_in_body = torch.tensor(attns_in_body)
    return {"comment": attns_in_comment, "sig": attns_in_comment, "body": attns_in_body}

def mean_pool_attn_from_toks(model, toks):
    assert len(toks.size()) == 1, "mean pooling batched toks is currently not supported"
    enc = model(toks, output_attentions=True)
    attns = enc["attentions"]
    # quite a deep tensor...
    layer_i = 0
    batch_i = 0 # we only have one prompt
    attn_head_i = 0
    # attns[layer_i][batch_i][attn_head_i][tok]

    # get last layer attns
    last_layer_attns = attns[-1][batch_i]
    last_layer_attns_head_mean = last_layer_attns.mean(dim=0)

    ar = torch.flip(torch.arange(1, len(enc[0]) + 1), [0]).cuda()
    summed = last_layer_attns_head_mean.sum(0)
    mean_pooled = summed / ar
    return mean_pooled


def find_end_tok_i(tokenizer, enc, stop_seqs=["\nend", "\n--"]):
    def stop_in_enc(enc):
        dec = tokenizer.decode(enc)
        for stop in stop_seqs:
            if stop in dec:
                return True

        return False
        
    i = 0
    
    while i < len(enc) - 1 and not stop_in_enc(enc[:i]):
        i += 1

    return i

def generate_with_stop(model, tokenizer, prompt):
    toks = tokenizer.encode(prompt, return_tensors="pt").to(model.device)
    out = model.generate(toks, use_cache=True, do_sample=True, max_new_tokens=500, temperature=0.2, top_p=0.95)
    end_tok = find_end_tok_i(tokenizer, out[0][len(toks[0])-1:]) + len(toks[0])
    out = out[0][:end_tok-1]
    return out