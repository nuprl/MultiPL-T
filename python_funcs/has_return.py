from tree_sitter_parser import LANGUAGE, global_parser

RETURN_QUERY = LANGUAGE.query("""
(return_statement) @return
""")


def does_have_return(src):
    tree = global_parser.parse(bytes(src, "utf8"))
    root = tree.root_node
    captures = RETURN_QUERY.captures(root)
    for node, _ in captures:
        # if it doesn't have an argument, it's not a return with a value
        if len(node.children) <= 1: # includes "return" itself
            continue
        else:
            return True


    return False


if __name__ == "__main__":
    print(does_have_return("def foo():\n    return"))
