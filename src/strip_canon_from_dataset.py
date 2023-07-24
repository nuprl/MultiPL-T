import datasets 
import argparse
import re

def racket_strip_cannon(ex):
    content_lines = ex["content"].split("\n")
    # regex to recognize linestart ;; #
    canon_regex = re.compile(r"^\s*;;\s*#.*$")
    new_content = []
    cannon = []
    for line in content_lines:
        if re.match(canon_regex, line):
            cannon.append(line)
        else:
            new_content.append(line)
    ex["content"] = "\n".join(new_content)
    if len(cannon) > 0:
        ex["cannon"] = "\n".join(cannon)
    else:
        ex["cannon"] = None
    return ex
    


    

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset", type=str, required=True, help="Dataset name")
    parser.add_argument("--language", type=str, required=True, help="Language of the dataset")
    parser.add_argument("--push", action="store_true", help="Push to HuggingFace Datasets")
    parser.add_argument("--local-file", type=str, help="Local file to save in addition/instead-of to pushing to HuggingFace Datasets")
    args = parser.parse_args()
    if not args.push and args.local_file is None:
        print("Must specify --push or --local-file")
        exit(1)
    
    dataset = datasets.load_dataset(args.dataset)
    if args.language == "racket":
        dataset = dataset.map(racket_strip_cannon)
    else:
        # crash with unimplemented language
        raise NotImplementedError(f"Language {args.language} not implemented")
    if args.push:
        dataset.push_to_hub(args.dataset)
    else:
        dataset.save_to_disk(args.local_file)