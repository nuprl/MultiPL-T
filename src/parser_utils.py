from LangParser import LangParser
from tree_sitter import Node

def node_to_string(src: bytes, node: Node) -> str:
    return src[node.start_byte:node.end_byte].decode("utf8")

def count_total_nodes(node: Node) -> int:
    count = 0
    stack = [node]
    while stack:
        node = stack.pop()
        count += 1
        stack.extend(node.children)
    return count

        

