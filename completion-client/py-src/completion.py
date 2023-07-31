import asyncio
import aiohttp

async def make_single_request(prog, session, server_url):
    msg_dict = { 
        "inputs": prog["prompt"], 
        "parameters": 
            {
                "best_of": 1,
                "decode_input_details": False, 
                "details": False, 
                "do_sample": True, 
                "max_new_tokens": 64,
                "repetition_penalty": None,
                "return_full_text": None,
                "seed": None,
                "stop": prog["stop_tokens"],
                "temperature": 0.8,
                "truncate": None,
                "top_k": None,
                "top_p": 0.95,
                "typical_p": None, 
                "watermark": False
            }
        }
    async with session.post(server_url, json=msg_dict) as resp:
        if resp.status == 200:
            resp_json = await resp.json()
            return resp_json["generated_text"]
        else:
            return None 

async def make_requests(compl_queue, run_queue, server_url):
   async with aiohttp.ClientSession() as session:
        while True:
            prog = await compl_queue.get()
            if prog is None:
                break
            else:
                completion = await make_single_request(prog, session, server_url)
                if completion is not None:
                    prog["completion"] = completion
                    await run_queue.put(prog)
 
async def spawn_connections(num_connections, compl_queue, run_queue, server_url):
    with asyncio.TaskGroup() as g:
        for _ in range(num_connections):
            g.create_task(make_requests(compl_queue, run_queue, server_url))

