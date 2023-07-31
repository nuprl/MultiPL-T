import asyncio
from typing import AsyncGenerator, Set
import aiofiles
from pathlib import Path
import json

async def jsonl_iter(jsonl_file: Path, open_mode):
    async with aiofiles.open(str(jsonl_file), open_mode) as f:
        async for line in f:
            yield json.loads(line)


def preload(out_file: Path) -> Set[str]:
    seen = set()
    with open(str(out_file), "r") as f:
        for line in f:
            prog = json.loads(line)
            seen.add(prog["name"]) 
    return seen

async def read_prompts(prompt_file: Path, seen: Set[str], compl_queue): 
    async for prog in jsonl_iter(prompt_file, "r"):
        prog["attempts"] = 0
        prog["completion"] = ""
        if prog["name"] not in seen:
            await compl_queue.put(prog)

async def write_completions(out_file: Path, fin_queue):
    async with aiofiles.open(str(out_file), "a") as f:
        while True:
            prog = await fin_queue.get()
            await f.write(json.dumps(prog) + "\n")