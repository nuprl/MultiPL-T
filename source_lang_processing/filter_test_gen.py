import datasets
import signal
from tree_sitter_parser import global_parser, LANGUAGE, node_to_string
import argparse
from testright.implant import implant
import multiprocessing
import progressbar
import benchmark_data

parser = argparse.ArgumentParser()
parser.add_argument('--dataset', type=str,
                    default='nuprl/stack-dedup-python-testgen-starcoder')
parser.add_argument('--result', type=str,
                    default='nuprl/stack-dedup-python-testgen-starcoder-filter')
parser.add_argument('--inference', action='store_true')
parser.add_argument('--batch_size', type=int, default=20)
args = parser.parse_args()


def isEnglish(s):
    try:
        s.encode(encoding='utf-8').decode('ascii')
    except UnicodeDecodeError:
        return False
    else:
        return True


FN_BLOCK_QUERY = LANGUAGE.query("""
(function_definition
  body: (block) @fn-block)
""")


ds = datasets.load_dataset(
    args.dataset,
    split='train',
)

num_todo = 0
num_only_return = 0
num_benchmark = 0
num_bad_docstring = 0
num_failed_inference = 0
not_english = 0
num_left = 0


def count_total_nodes(node):
    return 1 + sum(count_total_nodes(c) for c in node.children)


new_ds = {
    "content": [],
    "content_with_types": [],
    "sha1": [],
    "id": [],
    "entrypoint": [],
    "tests": [],
    "coverage": [],
    "tests_failed": [],
}


def run_inference(tpl):
    code_with_tests, entrypoint = tpl
    try:
        return implant(code_with_tests, entrypoint)
    except Exception as e:
        print(e)
        return None


bench_filter = benchmark_data.filter_out()
batch = []
p = multiprocessing.Pool(args.batch_size)
for i, ex in enumerate(progressbar.progressbar(ds)):
    if len(batch) < args.batch_size:
        ex["content_with_types"] = None
        batch.append(ex)
        continue

    if args.inference:
        codes_with_tests = [(code + "\n" + "\n".join(tests), entrypoint) for code, tests, entrypoint in zip(
            [ex['content'] for ex in batch], [ex['tests'] for ex in batch], [ex['entrypoint'] for ex in batch])]
        results = p.imap(run_inference, codes_with_tests)

        batch_idx = 0
        while True:
            try:
                def timeout_handler(_, __):
                    raise KeyboardInterrupt  # it's fineeeeeee
                signal.signal(signal.SIGALRM, timeout_handler)
                signal.alarm(5)
                result = next(results)
                batch[batch_idx]['content_with_types'] = result
                signal.alarm(0)
            except KeyboardInterrupt:
                signal.alarm(0)
                print("Keyboard interrupt. Terminating pool")
                p.terminate()
                p.close()
                p = multiprocessing.Pool(args.batch_size)
                break
            except StopIteration:
                break
            except Exception as e:
                print(e)
            batch_idx += 1

        signal.alarm(0)

    for ex in batch:
        code = ex['content']
        code_bytes = code.encode('utf-8')

        # filter out TODOs
        if 'TODO' in code:
            num_todo += 1
            continue

        found_bench = False
        all_bench = bench_filter["human_eval_docstrings"] + \
            bench_filter["human_eval_solutions"] + \
            bench_filter["mbpp_docstrings"] + \
            bench_filter["mbpp_solutions"]
        for b in all_bench:
            if b in code:
                found_bench = True
                break

        if found_bench:
            num_benchmark += 1
            continue

        # filter out non-english
        if not isEnglish(code):
            not_english += 1
            continue

        tree = global_parser.parse(code_bytes)
        block, _ = FN_BLOCK_QUERY.captures(tree.root_node)[0]
        # filter out functions with only return statement and too short, possibly
        # not useful
        if block.child_count == 2 and block.children[1].type == 'return_statement':
            ret = block.children[1]
            if count_total_nodes(ret) < 9:  # too short
                num_only_return += 1
                continue

        # get the docstring, filter if not a docstring
        exp = block.children[0]
        if not exp.type == 'expression_statement' and not exp.children[0].type == 'string':
            num_bad_docstring += 1
            continue

        docstring = exp.children[0]
        docstring_text = docstring.text.decode('utf-8')
        if not docstring_text.startswith('"""') and not docstring_text.endswith('"""'):
            num_bad_docstring += 1
            continue

        new_ds['content'].append(code)
        content_with_types = ex['content_with_types']
        if content_with_types is None:
            num_failed_inference += 1
        new_ds['content_with_types'].append(content_with_types)
        new_ds['entrypoint'].append(ex['entrypoint'])
        new_ds['sha1'].append(ex['sha1'])
        new_ds['id'].append(ex['id'])
        new_ds['tests'].append(ex['tests'])
        new_ds['coverage'].append(ex['coverage'])
        new_ds['tests_failed'].append(ex['tests_failed'])
        num_left += 1

    batch = []

p.terminate()
p.close()

print(f'num_only_return: {num_only_return}')
print(f'num_todo: {num_todo}')
print(f'num_benchmark: {num_benchmark}')
print(f'not_english: {not_english}')
print(f'num_bad_docstring: {num_bad_docstring}')
print(f'num_failed_inference: {num_failed_inference}')
print(f'num_left: {num_left}')
print(f'total: {len(ds)}')

new_ds = datasets.Dataset.from_dict(new_ds)
new_ds.push_to_hub(args.result)
