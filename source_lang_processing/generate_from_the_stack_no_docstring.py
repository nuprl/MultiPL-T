from tree_sitter_parser import LANGUAGE, global_parser, make_parser, node_to_string
import datasets
    (expression_statement 
        (string 
            (string_start) @docstring.start
            (string_content)
            (string_end) @docstring.end)))) @function.def
import signal
import sys
from multiprocessing import Pool


if len(sys.argv) != 2:
    print("Usage: python main.py <num workers>")
    sys.exit(1)

NUM_WORKERS = int(sys.argv[1])


example_source_bytes = bytes(
    """
def foo():
    \"\"\"
    This is a docstring 1
    \"\"\"
    if bar:
        baz()
def bar():
    \"\"\"
    This is a docstring 2
    \"\"\"
    if foo:
        baz()
def baz():
    # this is not a docstring
    if foo:
        bar()
def bez():
    " this is not a docstring "
    if foo:
        bar()
def bruh():
    x = 3
    \"\"\"
    This is a docstring, but starts later
    \"\"\"
    if foo:
        bar()
def bazooka():
    def inner():
        \"\"\"
        This is a docstring 3
        \"\"\"
        if foo:
            bar()
    if foo:
        bar()
    """,
    "utf8")

example_tree = global_parser.parse(example_source_bytes)

print(example_tree.root_node.sexp())

TOPLEVEL_QUERY = LANGUAGE.query("""
(function_definition
  name: (identifier)
  body: (block)) @function.def
""")


def get_fns(src, tree):
    captures = TOPLEVEL_QUERY.captures(tree.root_node)
    res = []
    for capture in captures:
        node, ty = capture
        if ty != "function.def":
            continue
        # if the starting col is not 0, then it's not a top-level fn
        _, col = node.start_point
        if col != 0:
            continue
        res.append(node_to_string(src, node))
    return res


print(get_fns(example_source_bytes, example_tree))

ds = datasets.load_dataset("bigcode/the-stack-dedup",
                           data_dir="data/python", split="train")
funs = set()


def parse_ex(parser, ex):
    ex = ex["content"]
    try:
        buf = bytes(ex, "utf8")
        tree = parser.parse(buf)
        return get_fns(buf, tree)
    except:
        return []


# if one parser segfaults, we can just make a new one and other parsers will still be fine
# WE LOVE TREE SITTER!
parsers = [make_parser() for _ in range(NUM_WORKERS)]


def process_chunk(idx_and_chunk):
    idx, chunk = idx_and_chunk
    parser = parsers[idx]
    chunk_new_funs = set()
    for ex in chunk:
        chunk_new_funs.update(parse_ex(parser, ex))
    return chunk_new_funs


total_len = len(ds)
CHUNK_SIZE = 1000 * NUM_WORKERS

print(f"Total length: {total_len}")
print(f"Chunk size: {CHUNK_SIZE}")

chunk = []
p = Pool(NUM_WORKERS)
for i, ex in enumerate(ds):
    if i % (total_len // 100) == 0:
        print(f"{i}/{total_len}")
    try:
        chunk.append(ex)
        if len(chunk) == CHUNK_SIZE or i == total_len - 1:
            print(f"Processing chunk {i // CHUNK_SIZE}")
            # divide the chunk into NUM_WORKERS chunks
            subchunk_size = len(chunk) // NUM_WORKERS
            subchunks = [chunk[i:i + subchunk_size]
                         for i in range(0, len(chunk), subchunk_size)]
            new_funs_iter = p.imap(
                process_chunk, [(i, subchunk) for i, subchunk in enumerate(subchunks)])
            print("Getting new functions")
            len_before = len(funs)
            while True:
                try:
                    def timeout_handler(_, __):
                        raise KeyboardInterrupt  # it's fineeeeeee
                    signal.signal(signal.SIGALRM, timeout_handler)
                    signal.alarm(60)
                    funs.update(next(new_funs_iter))
                    signal.alarm(0)
                except KeyboardInterrupt:
                    signal.alarm(0)
                    print("Keyboard interrupt. Terminating pool")
                    p.terminate()
                    p = Pool(NUM_WORKERS)
                    break
                except StopIteration:
                    break
                except Exception as e:
                    print(e)

            signal.alarm(0)

            parsers = [make_parser() for _ in range(NUM_WORKERS)]

            print(
                f"Done processing chunk {i // CHUNK_SIZE}. Got {len(funs) - len_before} new functions")

            chunk = []
    except Exception as e:
        print(e)
        chunk = []

    if i == total_len - 1:
        break

p.close()


new_ds_dict = {
    "content": list(funs),
    "id": list(range(len(funs)))
}

new_ds = datasets.Dataset.from_dict(new_ds_dict)
new_ds.push_to_hub("nuprl/stack-dedup-python-fns-not-documented")
