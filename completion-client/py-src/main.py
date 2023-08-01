import argparse
import asyncio

from reader_writer import preload, read_prompts, write_completions
from runner import spawn_runners
from completion import spawn_connections

def cli():
    args = argparse.ArgumentParser()
    args.add_argument("--prompt-file", type=str, required=True)
    args.add_argument("--out-file", type=str, required=True)
    args.add_argument("--num-connections", type=int, required=True)
    args.add_argument("--num-runners", type=int, required=True)
    args.add_argument("--attempt-limit", type=int, required=True)
    args.add_argument("--endpoint-url", type=str, required=True)

    return args.parse_args()

async def main():
    args = cli()
    seen = preload(args.out_file)
    run_queue = asyncio.Queue()
    compl_queue = asyncio.Queue()
    fin_queue = asyncio.Queue()
    await read_prompts(args.prompt_file, seen, compl_queue)
    await spawn_connections(args.num_connections, args.endpoint_url, run_queue, compl_queue, fin_queue)
    await spawn_runners(run_queue, compl_queue, fin_queue, args.attempt_limit, args.num_runners)
    await write_completions(args.out_file, fin_queue)

if __name__ == "__main__":
    asyncio.run(main())