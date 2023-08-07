from typing import List, Tuple
from tree_sitter_languages import get_language

LANGUAGE = get_language('python')

ARGS_QUERY = LANGUAGE.query(f"""
(assert_statement (comparison_operator (call function: (identifier) arguments: (argument_list (_) @args)) "==" (_)))
""")


Assertion = Tuple[List[bytes], bytes]
AssertionGroup = List[Assertion]


def capture_assertions(fn_name, node) -> AssertionGroup:
    ASSERTION_QUERY = LANGUAGE.query(f"""
        (
            (assert_statement (comparison_operator (call function: (identifier) @fn-name arguments: (argument_list (_))) "==")) @assert
            (#eq? @fn-name "{fn_name}")
        )
        """)
    RET_QUERY = LANGUAGE.query(f"""
        (assert_statement (comparison_operator (call function: (identifier) arguments: (argument_list (_))) "==" (_) @ret))
        """)
    res = []
    for cap, typ in ASSERTION_QUERY.captures(node):
        if typ == "assert":
            this_res = []
            args = ARGS_QUERY.captures(cap)
            for arg, _ in args:
                this_res.append(arg.text)
            ret = RET_QUERY.captures(cap)[0][0]
            res.append((this_res, ret.text))

    return res
