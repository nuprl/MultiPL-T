from tree_sitter_parser import *

KWARGS_QUERY = LANGUAGE.query("""
(argument_list (keyword_argument)) @kwarg
""")


def capture_assertions(fn_name, node):
    # 1. Find all assert statements
    # 2. Make sure they are in the format `assert {fn_name}(...) == ...`
    # 3. Filter out ones without at least 1 arguments
    # 5. Filter out ones with keyword arguments
    ASSERTION_QUERY = LANGUAGE.query(f"""
    (
        (assert_statement (comparison_operator (call function: (identifier) @fn-name arguments: (argument_list (_))) "==")) @assert
        (#eq? @fn-name "{fn_name}")
    )
    """)
    for cap, typ in ASSERTION_QUERY.captures(node):
        if typ == "assert" and len(KWARGS_QUERY.captures(cap)) == 0:
            yield cap


def assert_block_start_prelude(func_name):
    prompt = f"""# Unit tests for {func_name}
# These unit tests are strictly in the `assert {func_name}(...) == ...` format.
# Additionally, these unit tests are not allowed to use keyword arguments.\n"""
    return f"\n\n{prompt}"


def assert_block_start(func_name):
    return f"{assert_block_start_prelude(func_name)}assert {func_name}("


if __name__ == "__main__":
    assert_code = """
    assert 1 == 1
    assert bleh() == 1
    assert bleh(1, 2) == 1
    assert bleh(1, 2) == 1, "message"
    assert bleh(1, 2) != 1, "message"
    assert not bleh(1, 2) == 1, "message"
    assert bleh(a=1, 2) == 1
    """
    assert_bytes = bytes(assert_code, "utf-8")
    assert_tree = global_parser.parse(assert_bytes)
    print("TREEEEEEE")
    print(assert_tree.root_node.sexp())
    print("CAPTURESSSSSSSSSS")
    for capture in capture_assertions("bleh", assert_tree.root_node):
        strin = node_to_string(assert_bytes, capture)
        print(f"{strin} ----> {capture.sexp()}")
