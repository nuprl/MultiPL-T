from tree_sitter import Language, Parser

Language.build_library(
    'build/lang.so',
    [
        '../tree-sitter-python'
    ]
)
LANGUAGE = Language('build/lang.so', 'python')


QUERY = LANGUAGE.query("""
(function_definition name: (identifier) @fn-name)
""")
def get_fn_name(code):
    src = bytes(code, "utf8")
    tree = global_parser.parse(src)
    node = tree.root_node
    for cap, typ in QUERY.captures(node):
        if typ == "fn-name":
            return node_to_string(src, cap)
    return None


def node_to_string(src: bytes, node):
    return src[node.start_byte:node.end_byte].decode("utf8")


def make_parser():
    _parser = Parser()
    _parser.set_language(LANGUAGE)
    return _parser


global_parser = Parser()
global_parser.set_language(LANGUAGE)

if __name__ == "__main__":
    code = """
import ble
from a import b
"""
    print(global_parser.parse(bytes(code, "utf8")).root_node.sexp())
