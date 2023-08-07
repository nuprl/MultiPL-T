from tree_sitter_parser import LANGUAGE, global_parser, node_to_string
import json
import datasets
import hashlib
import tempfile
import os
import subprocess
import autopep8


def format_python(code):
    return autopep8.fix_code(code, options={"max_line_length": 1000})


def clean_cg(obj, funcs, filename_no_py):
    for key in list(obj.keys()):
        delete = True
        for name in funcs.keys():
            if name in key:
                delete = False
                break

        if not key.startswith(filename_no_py):
            delete = True

        if "<lambda" in key:
            delete = True

        if key == filename_no_py:  # also remove the filename itself
            delete = True

        if delete:
            del obj[key]


# this is not perfect. i don't plan to make it perfect.
def find_roots(cg):
    # if they have no parents, they are roots.
    # set that all are roots, then we go by and remove the ones that are not
    roots = set([k for k in cg.keys()])
    for k, v in cg.items():
        for child in v:
            if child == k:
                # may be recursive, so we can't just remove it if it refers to itself
                continue

            roots.discard(child)

    return list(roots)


def has_errors(code):
    try:
        compile(code, "test", "exec")
    except SyntaxError:
        return True
    return False

# plan is to put all the non-root fns into the root fn as local fns
# we find the root fn using a call graph


def merge_fns(code, funcs):
    print(f"Merging {len(funcs)} functions into one B^)")
    with tempfile.NamedTemporaryFile(suffix=".py") as f:
        # add fake calls to the functions so that they are processed by pycg
        filename_no_py = f.name[:-3].split("/")[-1]
        code_mocked = code + "\n"
        for name in funcs.keys():
            code_mocked += f"{name}()\n"

        f.write(code_mocked.encode("utf8"))
        f.flush()
        p = subprocess.run(
            ["pycg", f.name], capture_output=True, cwd=os.path.dirname(f.name))

        try:
            obj = json.loads(p.stdout)
        except:
            print("*** Skipping code with pycg error")
            return None

    clean_cg(obj, funcs, filename_no_py)
    print(obj)
    roots = [root.split(f"{filename_no_py}.")[1]
             for root in find_roots(obj)]

    if len(roots) == 0 or len(roots) > 2:
        print("*** Skipping code with wrong number of roots:", roots)
        return None

    root_name = roots[0]
    print("Root func name:", root_name)
    root_fn = funcs[root_name]
    funcs_without_root = [func for func in funcs.values() if func != root_fn]

    # now, we implant the helper functions into the root function
    root_fn_lines = root_fn.split("\n")
    assert root_fn_lines[0].startswith("def ")

    for non_root in funcs_without_root:
        non_root_lines = non_root.split("\n")
        assert non_root_lines[0].startswith("def ")
        # indent all lines
        non_root_lines = ["    " + line for line in non_root_lines]
        # insert all lines right after the def line
        root_fn_lines = root_fn_lines[:1] + \
            non_root_lines + root_fn_lines[1:]

    root_fn = "\n".join(root_fn_lines)
    return root_fn


TOPLEVEL_FN_NAME_QUERY = LANGUAGE.query("""
(module
    (function_definition name: (identifier) @fn-name) @fn
)
""")


def get_fns(code):
    src = bytes(code, "utf8")
    tree = global_parser.parse(src)
    node = tree.root_node
    names = []
    funcs = []
    for cap, typ in TOPLEVEL_FN_NAME_QUERY.captures(node):
        if typ == "fn-name":
            names.append(node_to_string(src, cap))
        elif typ == "fn":
            funcs.append(node_to_string(src, cap))
    # make a map name -> func
    funcs = {name: func for name, func in zip(names, funcs)}
    return funcs


TOPLEVEL_IMPORT_QUERY = LANGUAGE.query("""
(module
    (import_statement) @import
)
""")
TOPLEVEL_FROM_IMPORT_QUERY = LANGUAGE.query("""
(module
    (import_from_statement) @import
)
""")


def get_imports(code):
    src = bytes(code, "utf8")
    tree = global_parser.parse(src)
    node = tree.root_node
    imports = []
    for cap, typ in TOPLEVEL_IMPORT_QUERY.captures(node):
        if typ == "import":
            imports.append(node_to_string(src, cap))

    for cap, typ in TOPLEVEL_FROM_IMPORT_QUERY.captures(node):
        if typ == "import":
            imports.append(node_to_string(src, cap))

    return imports


# remove the follow-up section, may confuse the model
def clean_prompt(prompt):
    # is either "Follow-up" or "Follow up"...
    follow_up_start = prompt.find("**Follow")
    if follow_up_start == -1:
        return prompt

    # after the follow-up section there is nothing, so we can just remove it like this
    return prompt[:follow_up_start]


ds = datasets.load_dataset("mhhmm/leetcode-solutions-python", split="train")

new_ds = {
    "id": [],
    "sha1": [],
    "content": [],
    "difficulty": [],
    "problem_name_coded": [],
    "problem_name_worded": [],
    "full_code": [],
    "signature": [],
    "prompt": [],
    "imports": [],
    "was_merged": [],
}

e_id = 0
for ex in ds:
    full_thing = ex["code_with_data"]
    full_thing_lines = full_thing.split("\n")
    problem_name_coded = full_thing_lines[0].split("#")[1].lstrip()
    problem_name_worded = full_thing_lines[1].split("#")[1].lstrip()
    difficulty = full_thing_lines[2].split("#")[1].lstrip()
    # remove the "# " at the beginning of the first line
    full_thing_no_metadata = "\n".join(full_thing_lines[3:])[2:]
    prompt_end = full_thing_no_metadata.find("```python")
    prompt = clean_prompt(full_thing_no_metadata[:prompt_end])
    code_start = prompt_end + len("```python")
    code_end = full_thing_no_metadata.find("```", code_start + 1)
    code = format_python(full_thing_no_metadata[code_start:code_end])
    print(
        f"Problem: {problem_name_coded} ({problem_name_worded}) -- Difficulty: {difficulty} -- Prompt length: {len(prompt)} -- Code length: {len(code)}")
    if has_errors(code):
        print("*** Skipping code with syntax error")
        continue

    if "\nclass" in code:  # give up...
        print("*** Skipping code with class")
        continue

    funcs = get_fns(code)
    if len(funcs) == 0:
        # some issue
        print("*** Skipping code with no functions")
        continue

    main_func = None
    was_merged = False
    if len(funcs) == 1:
        # easy peasy
        main_func = list(funcs.values())[0]
    else:
        # omg, we gotta do this crazy call graph stuff + merging
        main_func = merge_fns(code, funcs)
        was_merged = True

    if main_func is None:  # merge_fns failed
        continue

    if has_errors(main_func):
        print("*** Skipping code with syntax error after merging")
        continue

    imports = get_imports(code)
    if len(imports) > 0:
        print("Imports:", imports)
    # alrighty, now we need to insert the inport statements into the main func, and add the docstring
    # indent by 4 spaces
    imports = ["    " + line for line in imports]
    prompt_lines = [
        "    " + line for line in ('"""\n' + prompt + '"""').split("\n")]
    # insert right after the def line
    main_func_lines = main_func.split("\n")
    main_func_lines = main_func_lines[:1] + prompt_lines + imports + \
        main_func_lines[1:]
    main_func = "\n".join(main_func_lines)
    if has_errors(main_func):
        print("*** Skipping code with syntax error after inserting imports")
        continue

    print(main_func)

    hash_object = hashlib.sha1(bytes(main_func, "utf8"))
    hex_dig = hash_object.hexdigest()

    # the function signature (including the docstring)
    sig_end = main_func.find('"""', main_func.find('"""') + 1) + 3
    signature = main_func[:sig_end]

    new_ds["id"].append(e_id)
    new_ds["sha1"].append(hex_dig)
    new_ds["content"].append(main_func)
    new_ds["difficulty"].append(difficulty)
    new_ds["problem_name_coded"].append(problem_name_coded)
    new_ds["problem_name_worded"].append(problem_name_worded)
    new_ds["full_code"].append(code)
    new_ds["imports"].append(imports)
    new_ds["signature"].append(signature)
    new_ds["prompt"].append(prompt)
    new_ds["was_merged"].append(was_merged)
    e_id += 1

new_ds = datasets.Dataset.from_dict(new_ds)
new_ds.push_to_hub("nuprl/leetcode-solutions-python-formatted")
