import argparse
import datasets

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=str, required=True)
    parser.add_argument("--equalize", action="store_true")

    args = parser.parse_args()
    lua_data = datasets.load_dataset("nuprl/multipl-t_lua_paper_aug", split="train")
    rkt_data = datasets.load_dataset("nuprl/multipl-t_rkt_paper_aug", split="train")
    ocaml_data = datasets.load_dataset("nuprl/multipl-t_ocaml_paper_aug", split="train")

    if args.equalize:
        min_len = min(len(lua_data), len(rkt_data), len(ocaml_data))
        lua_data = lua_data.shuffle().select(range(min_len))
        rkt_data = rkt_data.shuffle().select(range(min_len))
        ocaml_data = ocaml_data.shuffle().select(range(min_len))
        full_data = datasets.concatenate_datasets([lua_data, rkt_data, ocaml_data]).shuffle()
    else:
        full_data = datasets.concatenate_datasets([lua_data, rkt_data, ocaml_data]).shuffle()
    
    full_data.to_json(args.output)

