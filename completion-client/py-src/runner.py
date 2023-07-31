import asyncio
import json
from typing import Optional

async def _run_single_program(prog) -> Optional[str]:
    full_prog_text = f"{prog['prompt']}\n{prog['completion']}\n{prog['tests']}"
    args = [ "simple_eval.py","--prog-text",full_prog_text,"--lang", prog["lang"] ]
    proc = await asyncio.create_subprocess_exec(*args, stdout=asyncio.subprocess.PIPE, stderr=asyncio.subprocess.PIPE)
    stdout, _ = await proc.communicate()
    if proc.returncode == 0:
        return json.loads(stdout)["status"]
    else:
        return None

    

async def _run_programs(run_queue, compl_queue, fin_queue, attempt_limit):
    while True: 
        prog = await run_queue.get()
        res = await _run_single_program(prog)
        if res is not None:
            if res.lower() == "ok": 
                prog["completion"] = res
                await fin_queue.put(prog)
            else:
                if prog["attempts"] < attempt_limit:
                    prog["attempts"] += 1
                    await compl_queue.put(prog)
                else: 
                    print(f"Abandonning program {prog['name']}")
        else: 
            if prog["attempts"] < attempt_limit:
                prog["attempts"] += 1
                await compl_queue.put(prog)
            print(f"Error for prog: {prog['name']}")
            

async def spawn_runners(run_queue, compl_queue, fin_queue, attempt_limit, num_runners):
    with asyncio.TaskGroup() as g:
        for _ in range(num_runners):
            g.create_task(_run_programs(run_queue, compl_queue, fin_queue, attempt_limit))