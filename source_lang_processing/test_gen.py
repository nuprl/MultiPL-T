import datasets
from tree_sitter_parser import global_parser, node_to_string, get_fn_name
from assert_test import capture_assertions, assert_block_start
import time
from accelerate import Accelerator
import argparse
import json
import zmq
from code_exec_server.code_exec_reqs import exec_test, run_coverage
from codegen import HFCodeGen, GPTCodeGen

parser = argparse.ArgumentParser()
parser.add_argument("--model", type=str,
                    default="bigcode/starcoder")
parser.add_argument("--num_comps", type=int, default=5)
parser.add_argument("--dataset", type=str,
                    default="nuprl/stack-dedup-python-fns-returns-typechecks")
parser.add_argument("--batch_size", type=int, default=1)
parser.add_argument("--resume", type=str, default="")
parser.add_argument("--server", type=str, default="http://127.0.0.1:8000")
parser.add_argument("--dataset_aggregator", type=str,
                    default="tcp://127.0.0.1:5555")
parser.add_argument("--temp", type=float, default=0.2)
parser.add_argument("--load_in_8bit", action="store_true")
parser.add_argument("--seq_len", type=int, default=2048)
parser.add_argument("--min_coverage", type=int)
args = parser.parse_args()


ds = datasets.load_dataset(args.dataset, split="train")
# shuffle
ds = ds.shuffle()


accelerator = Accelerator()

# if openai model, use GPTCodeGen, else use HFCodeGen
if args.model == "gpt-4" or args.model == "gpt-3.5-turbo":
    codegen = GPTCodeGen(args.model)
else:
    codegen = HFCodeGen(args.model, accelerator,
                        args.seq_len, args.load_in_8bit)

accelerator.wait_for_everyone()  # wait for model to be loaded


def extract_assertions(parser, code, name):
    code_bytes = bytes(code, "utf-8")
    tree = parser.parse(code_bytes)
    return list(map(lambda n: node_to_string(code_bytes, n), capture_assertions(name, tree.root_node)))


context = zmq.Context()
socket = context.socket(zmq.REQ)
socket.connect(args.dataset_aggregator)

resume_set = set()
if args.resume:
    dataset = datasets.load_dataset(args.resume, split="train")
    if accelerator.is_main_process:
        req = {
            "type": "resume",
            "dataset": args.resume
        }
        json_obj = json.dumps(req)
        socket.send_string(json_obj)
        assert socket.recv_string() == "OK"
    resume_set = set(dataset["sha1"])


b_i = 0
e_i = 0
passing_examples = 0
total_assertions = 0
total_passing_assertions = 0
batch = []
for i, ex in enumerate(ds):
    if i % accelerator.num_processes != accelerator.process_index:
        continue

    def ex_print(msg, end="\n"):
        print(
            f"[proc={accelerator.process_index}, ex={ex['id']}] {msg}", end=end)

    if ex["sha1"] in resume_set:
        print(f"Skipping example {ex['sha1']}: in resume id set...")
        continue

    batch.append(ex)
    if len(batch) < args.batch_size:
        continue

    prompts = []
    names = []

    for ex in batch:
        code = ex["content"]
        name = get_fn_name(code)
        names.append(name)
        tests_start = assert_block_start(name)
        code_with_tests = code + tests_start
        this_prompts = [code_with_tests] * args.num_comps
        prompts.extend(this_prompts)

    start_time = time.time()
    ex_print(f"Generating example batch {b_i}: {names}")
    b_i += 1  # increment example batch idx

    batched_comps = codegen.code_complete(
        prompts, temp=args.temp)

    ex_print(
        f"Generated {len(batched_comps)} completions in {time.time() - start_time} seconds")

    for i, ex in enumerate(batch):
        name = names[i]
        ex_print(f"Evaluating example {e_i} (batch id = {i}): {name}")
        e_i += 1  # increment example idx
        code = ex["content"]
        assertions = set()
        comps = set(batched_comps[i*args.num_comps:(i+1)*args.num_comps])
        for comp in comps:
            assertions.update(extract_assertions(
                global_parser, comp, name))

        total_assertions += len(assertions)

        passing_assertions = set()
        for assertion in assertions:
            if exec_test(args.server, code, assertion):
                passing_assertions.add(assertion)

        total_passing_assertions += len(passing_assertions)

        ex_print(
            f"Passing assertions (len: {len(passing_assertions)}): {passing_assertions}")

        if len(passing_assertions) == 0:
            ex_print("Skipping example. No passing assertions...")
            continue

        # get coverage
        passing_assertions_list = list(passing_assertions)
        coverage = run_coverage(args.server, code, passing_assertions_list)
        ex_print(f"Coverage: {coverage}")
        if coverage < 0 or (args.min_coverage and coverage < args.min_coverage):
            ex_print("Skipping example. Coverage failed...")
            continue

        passing_examples += 1

        req = {k: v for k, v in ex.items()}
        req["content"] = code
        req["entrypoint"] = name
        req["tests"] = passing_assertions_list
        req["tests_failed"] = list(assertions - passing_assertions)
        req["coverage"] = coverage
        # this is just info for the server
        req["type"] = "new"
        req["process_id"] = accelerator.process_index

        json_obj = json.dumps(req)
        socket.send_string(json_obj)
        assert socket.recv_string() == "OK"

        ex_print(
            f"Sent example {ex['id']} to server. "
            + f"Example pass rate: {passing_examples / e_i * 100:.2f}%. "
            + f"Assertion pass rate: {len(passing_assertions) / len(assertions) * 100:.2f}%. "
            + f"Overall assertion pass rate: {total_passing_assertions / total_assertions * 100:.2f}%."
        )

    batch = []

print(f"[{accelerator.process_index}] DONE! Asking server to exit...")
req = {
    "type": "exit",
    "process_id": accelerator.process_index
}
json_obj = json.dumps(req)
socket.send_string(json_obj)
assert socket.recv_string() == "BYE"