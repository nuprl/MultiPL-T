import json
from pathlib import Path
from typing import Generator, Union

def read_jsonl(path: Union[str, Path]) -> Generator[dict, None, None]:
    with open(path) as f:
        for line in f:
            yield json.loads(line)
