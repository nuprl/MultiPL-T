# A NumTree is either a Leaf or a Node
# A Leaf can be represented as {"type": "Leaf", "value": <Number>}
# A Node can be represented as {"type": "Node", "value": <Number>, "left": <NumTree>, "right": <NumTree>}

# mirror: NumTree -> NumTree
# Mirrors the tree around the center point
def mirror(tree):
    # <solution>
    if tree["type"] == "Leaf":
        return {"type": "Leaf", "value": tree["value"]}
    elif tree["type"] == "Node":
        return {"type": "Node", "value": tree["value"], "left": mirror(tree["right"]), "right": mirror(tree["left"])}
    


# <tests>
def assertions():
    assert (mirror({"type": "Leaf", "value": 5}) == {"type": "Leaf", "value": 5})
    assert (mirror({"type": "Node", "value": 5, "left": {"type": "Leaf", "value": 6}, "right": {"type": "Leaf", "value": 7}}) ==
                    {"type": "Node", "value": 5, "left": {"type": "Leaf", "value": 7}, "right": {"type": "Leaf", "value": 6}})
    assert (mirror({"type": "Node", "value": 5, "left": {"type": "Node", "value": 8, "left": {"type": "Leaf", "value": 9}, "right": {"type": "Leaf", "value": 10}}, "right": {"type": "Node", "value": 4, "left": {"type": "Leaf", "value": 3}, "right": {"type": "Leaf", "value": 2}}}) ==
                    {"type": "Node", "value": 5, "left": {"type": "Node", "value": 4, "left": {"type": "Leaf", "value": 2}, "right": {"type": "Leaf", "value": 3}}, "right": {"type": "Node", "value": 8, "left": {"type": "Leaf", "value": 10}, "right": {"type": "Leaf", "value": 9}}})

assertions()
