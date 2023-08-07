from tree_sitter_languages import get_parser
from testright.assertions import capture_assertions, LANGUAGE
from testright.inference import infer_assertions, required_imports

DEF_QUERY = LANGUAGE.query("""
(function_definition) @function.def
""")

IDENTIFIER_QUERY = LANGUAGE.query("""
(identifier) @identifier
""")

FN_DECL = LANGUAGE.query("""
(function_definition
  name: (identifier)
  parameters: (parameters (_) @param)
  return_type: (_)? @ret_type
  ":" @colon
  )
""")


def format_python(code):
    import autopep8
    return autopep8.fix_code(code, options={"max_line_length": 1000})


def implant(code: str | bytes, call_name: str) -> str:
    if isinstance(code, str):
        code = code.encode("utf8")
    tree = get_parser("python").parse(code)
    root = tree.root_node
    asserts = capture_assertions(call_name, root)
    arg_types, ret_type = infer_assertions(asserts)
    imports = required_imports(arg_types + [ret_type])

    # inplant into code
    # get first fn def
    fn_def = DEF_QUERY.captures(root)[0][0]
    # capture params and colon
    param_idx = 0
    shift = 0
    for cap, typ in FN_DECL.captures(fn_def):
        if typ == "param" and cap.type != "typed_parameter":
            # get first identifier
            identifier = IDENTIFIER_QUERY.captures(cap)[0][0]
            type_bytes = str(arg_types[param_idx]).encode("utf8")
            code = code[:identifier.end_byte+shift] + b": " + \
                type_bytes + code[identifier.end_byte+shift:]
            shift += len(type_bytes) + 2
            param_idx += 1
        elif typ == "ret_type":
            break
        elif typ == "colon":
            # insert ret
            ret_bytes = str(ret_type).encode("utf8")
            code = code[:cap.end_byte+shift-1] + b" -> " + \
                ret_bytes + b":" + code[cap.end_byte+shift:]
            break

    code = code.decode("utf8")
    import_line = ""
    if len(imports) > 0:
        import_line = "from typing import " + \
            ", ".join(list(imports)) + "\n"

    return format_python(import_line + code)
