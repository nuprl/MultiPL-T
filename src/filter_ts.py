import datasets 
import signal
from typing import List 
from tree_sitter import Parser, Language
from parser_utils import node_to_string, count_total_nodes 
from multiprocessing import Pool
from tqdm import tqdm

TIMEOUT = 15
NUM_PROCS = 7
CHUNK_SIZE = 1000
# Pickling parsers is a tricky game, so we just don't do it. 
PARSERS = []


'''
Given a program encoded as utf-8 bytes, return a list of functions preceded 
by a comment
'''
def find_comment_pairs(code: bytes, parser) -> List[str]:
    func_comment_pairs = []
    previous_comment = None 
    parsed_tree = parser.parse(code)
    cursor = parsed_tree.root_node.walk()
    if not cursor.goto_first_child():
        return func_comment_pairs
    while True: 
        if previous_comment is None: 
            if cursor.node.type == 'comment':
                previous_comment = node_to_string(code, cursor.node)
        else:
            if cursor.node.type == 'comment':
                previous_comment += node_to_string(code, cursor.node)
            elif cursor.node.type == 'function_declaration':
                func_str =  node_to_string(code, cursor.node)
                func_comment_pairs.append(f"{previous_comment.rstrip()}\n{func_str.rstrip()}")
                previous_comment = None
            else: 
                previous_comment = None
        if not cursor.goto_next_sibling(): 
            break
    return func_comment_pairs


def parse_ex(parser, ex) -> bool:
    try:
        return find_comment_pairs(bytes(ex["content"], "utf8"), parser)
    except Exception:
        return []

def make_parser(lang):
    parser = Parser()
    parser.set_language(Language('build/languages.so', lang))
    return parser

def parse_chunk(idx_chunk):
    global PARSERS
    idx, chunk = idx_chunk
    parser = PARSERS[idx % NUM_PROCS]
    chunk_new_funs = set()
    for ex in chunk:
        chunk_new_funs.update(parse_ex(parser, ex))
    return chunk_new_funs

def proc_chunk(chunk):
    global PARSERS
    funcs = set()
    PARSERS = [make_parser("typescript") for _ in range(NUM_PROCS)]
    subchunk_size = len(chunk) // NUM_PROCS
    subchunks = [chunk[i:i + subchunk_size] for i in range(0, len(chunk), subchunk_size)]
    p = Pool(NUM_PROCS)
    new_pairs_iter = p.imap(parse_chunk, enumerate(subchunks))
    while True:
        try:
            def timeout_handler(_, __):
                raise KeyboardInterrupt  # it's fineeeeeee
            signal.signal(signal.SIGALRM, timeout_handler)
            signal.alarm(TIMEOUT)
            funcs.update(next(new_pairs_iter))
            signal.alarm(0)
        except KeyboardInterrupt:
            signal.alarm(0)
            print("Keyboard interrupt. Terminating pool")
            p.terminate()
            p.close()
            p = Pool(NUM_PROCS)
            break
        except StopIteration:
            break
        except Exception as e:
            print(e)
    signal.alarm(0)
    p.close()
    return funcs

if __name__ == "__main__":
    RAW_TS_DATASET = datasets.load_dataset(
        "bigcode/the-stack-dedup", data_dir="data/typescript", split="train"
    )
    total_len = len(RAW_TS_DATASET)
    all_funcs = set()
    chunk = []
    for i, ex in tqdm(enumerate(RAW_TS_DATASET), total=RAW_TS_DATASET.num_rows):
        chunk.append(ex)
        if len(chunk) == CHUNK_SIZE or i == total_len - 1:
            all_funcs.update(proc_chunk(chunk))
            chunk = []

    new_ds_dict = { 
        "content": list(all_funcs),
        "id": list(range(len(all_funcs)))
    }
    new_ds = datasets.Dataset.from_dict(new_ds_dict)
    new_ds.push_to_hub("nuprl/stack-dedup-typescript-functions")
    
     