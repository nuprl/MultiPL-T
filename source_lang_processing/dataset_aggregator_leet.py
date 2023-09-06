import datasets
import json
import argparse
import zmq

parser = argparse.ArgumentParser()
parser.add_argument("--name", type=str,
                    default="nuprl/leetcode-solutions-python-testgen")
parser.add_argument("--port", type=int, default=5556)
args = parser.parse_args()

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind(f"tcp://0.0.0.0:{args.port}")

new_ds = {
    "content": [],
    "sha1": [],
    "id": [],
    "entrypoint": [],
    "tests": [],
    "coverage": [],
    "difficulty": [],
    "topics": [],
    "problem_name_coded": [],
    "prompt": [],
    "imports": [],
    "was_merged": [],
    "tests_failed": [],
}

sha1_dedup = set()
client_ids = set()


def push():
    new_ds_hf = datasets.Dataset.from_dict(new_ds)
    try:
        new_ds_hf.push_to_hub(args.name)
    except Exception as e:
        print(e)


num = 0
while True:
    print("Waiting for request...")
    message = socket.recv()
    req = json.loads(message)
    if req["type"] == "exit":
        pid = req["process_id"]
        print(f"Process {pid} exited")
        client_ids.remove(pid)
        socket.send(b"BYE")
        if len(client_ids) == 0:
            push()
            break
    elif req["type"] == "resume":
        dataset = datasets.load_dataset(req["dataset"], split="train")
        print(f"Resuming from {len(dataset)} examples")
        new_ds["content"] = list(dataset["content"])
        new_ds["sha1"] = list(dataset["sha1"])
        sha1_dedup = set(new_ds["sha1"])
        new_ds["id"] = list(dataset["id"])
        new_ds["entrypoint"] = list(dataset["entrypoint"])
        new_ds["tests"] = list(dataset["tests"])
        new_ds["coverage"] = list(dataset["coverage"])
        new_ds["difficulty"] = list(dataset["difficulty"])
        new_ds["problem_name_coded"] = list(dataset["problem_name_coded"])
        new_ds["topics"] = list(dataset["topics"])
        new_ds["prompt"] = list(dataset["prompt"])
        new_ds["imports"] = list(dataset["imports"])
        new_ds["was_merged"] = list(dataset["was_merged"])
        new_ds["tests_failed"] = list(dataset["tests_failed"])
        num = 0
        socket.send(b"OK")
    elif req["type"] == "new":
        pid = req["process_id"]
        client_ids.add(pid)
        sha1 = req["sha1"]
        if sha1 in sha1_dedup:
            socket.send(b"OK")
            continue
        sha1_dedup.add(sha1)
        entrypoint = req["entrypoint"]
        content = req["content"]
        tests = req["tests"]
        coverage = req["coverage"]
        id_ = req["id"]
        difficulty = req["difficulty"]
        topics = req["topics"]
        problem_name_coded = req["problem_name_coded"]
        prompt = req["prompt"]
        imports = req["imports"]
        was_merged = req["was_merged"]
        tests_failed = req["tests_failed"]

        new_ds["content"].append(content)
        new_ds["sha1"].append(sha1)
        new_ds["id"].append(id_)
        new_ds["entrypoint"].append(entrypoint)
        new_ds["coverage"].append(coverage)
        new_ds["tests"].append(tests)
        new_ds["difficulty"].append(difficulty)
        new_ds["problem_name_coded"].append(problem_name_coded)
        new_ds["topics"].append(topics)
        new_ds["prompt"].append(prompt)
        new_ds["imports"].append(imports)
        new_ds["was_merged"].append(was_merged)
        new_ds["tests_failed"].append(tests_failed)

        print(
            f"Process {pid} added new data. Problem {entrypoint} (id: {id_}, {len(tests)} tests). Current size: {len(new_ds['content'])}")

        # push every 10 examples
        if num % 10 == 0:
            push()

        num += 1
        socket.send(b"OK")
    else:
        socket.send(b"ERROR")
