from typing import Dict, List, Set, Union, Tuple, Optional

from testright.assertions import AssertionGroup


class Annot:
    def __init__(self, name, children: Union[Set, List] = [], improvable=False):
        if name == "Union":
            assert isinstance(
                children, set), "Union types must have a set of children"

        self.name = name
        self.children = children
        self.improvable = improvable

    def __str__(self):
        if len(self.children) == 0:
            return self.name
        else:
            return f'{self.name}[{", ".join(map(repr, self.children))}]'

    def __repr__(self):
        return str(self)

    def __eq__(self, other):
        if self.name == "Union" and other.name == "Union":
            return set(self.children) == set(other.children)
        return self.name == other.name and self.children == other.children

    def __hash__(self):
        return hash((self.name, tuple(self.children)))


def get_type_annot(value):
    name = type(value).__name__

    def unionify_if_needed(types: Set):
        if len(types) == 1:
            return types.pop()
        else:
            # NOTE: keeps types as set
            return Annot("Union", children=types)

    def get_for_collection(collection):
        if len(value) == 0:
            return Annot(collection, improvable=True)
        else:
            return Annot(collection, children=[unionify_if_needed(set(map(get_type_annot, value)))])

    match str(name):
        case 'int':
            return Annot("int")
        case 'float':
            return Annot("float")
        case 'str':
            return Annot("str")
        case 'NoneType':
            return Annot("None")
        case 'bool':
            return Annot("bool")
        case 'list':
            return get_for_collection("List")
        case 'tuple':
            if len(value) == 0:
                return Annot("Tuple", improvable=True)
            else:
                return Annot("Tuple", children=list(map(get_type_annot, value)))
        case 'set':
            return get_for_collection("Set")
        case 'dict':
            if len(value) == 0:
                return Annot("Dict", improvable=True)
            else:
                key_type = unionify_if_needed(
                    set(map(get_type_annot, value.keys())))
                value_type = unionify_if_needed(
                    set(map(get_type_annot, value.values())))
                return Annot("Dict", children=[key_type, value_type])

    return Annot(name)


def resolve_improvables(annot: Annot) -> Annot:
    for child in annot.children:
        if child.improvable:
            rem = [c for c in annot.children if c != child]
            if any(c.name == child.name for c in rem):
                if isinstance(child.children, set):
                    rem = set(rem)
                annot.children = rem
        else:
            resolve_improvables(child)
    return annot


def unify_annotations(annots: List[Annot], debug=-1) -> Annot:
    def d_print(*args, **kwargs):
        if debug >= 0:
            print(debug, *args, **kwargs)

    assert len(annots) > 0
    next_debug = debug + 1 if debug >= 0 else -1
    unified = annots[0]
    for annot in annots:
        d_print(f"unifying {unified} and {annot}")
        if annot != unified:
            d_print("not equal, unifying")
            if unified.name == "Union":
                d_print("unified is a union")
                assert isinstance(unified.children, set)
                if annot.name == "Union":
                    unified.children.update(annot.children)
                else:
                    unified.children.add(annot)
            elif annot.name == "Union":
                d_print("annot is a union")
                assert isinstance(annot.children, set)
                if unified.name == "Union":
                    annot.children.update(unified.children)
                else:
                    annot.children.add(unified)
                unified = annot
            elif unified.name == annot.name and len(unified.children) == len(annot.children):
                if unified.name in ["List", "Set"]:
                    d_print("unifying list or set")
                    unified_children = unify_annotations(
                        list(unified.children) + list(annot.children), debug=next_debug)
                    unified = Annot(unified.name, children=[unified_children])
                elif unified.name == "Tuple":
                    d_print("unifying tuple")
                    assert isinstance(unified.children, list)
                    assert isinstance(annot.children, list)
                    for i in range(len(unified.children)):
                        unified.children[i] = unify_annotations(
                            [unified.children[i], annot.children[i]], debug=next_debug)
                elif unified.name == "Dict":
                    d_print("unifying dict")
                    assert isinstance(unified.children, list)
                    assert isinstance(annot.children, list)
                    unified.children[0] = unify_annotations(
                        [unified.children[0], annot.children[0]], debug=next_debug)
                    unified.children[1] = unify_annotations(
                        [unified.children[1], annot.children[1]], debug=next_debug)
                else:
                    raise NotImplementedError(
                        "unifying same types: " + unified.name)
            else:
                d_print(
                    f"neither is a union. unified children: {unified.children} -- annot children: {annot.children}")
                unified = Annot("Union", children=set([unified, annot]))
    return unified


def simplify_union(annot: Annot) -> Annot:
    """
    If there is an Union[T, None] in the annotation (even in children, recursively), transform it to Optional[T]
    """
    if annot.name == "Union" and len(annot.children) == 2 and Annot("None") in annot.children:
        annot.children.remove(Annot("None"))
        annot.name = "Optional"
        annot.children = [annot.children.pop()]
    else:
        # Recursively simplify the union in child nodes
        for child in annot.children:
            simplify_union(child)

    return annot


def full_pizza(annots: List[Annot], debug=-1) -> Annot:
    return simplify_union(resolve_improvables(unify_annotations(annots, debug=debug)))


InferredFunctionType = Tuple[List[Annot], Annot]


def infer_assertions(asserts: AssertionGroup) -> InferredFunctionType:
    longest_call = len(max(asserts, key=lambda args: len(args[0]))[0])
    arg_types = [[] for _ in range(longest_call)]
    return_types = []

    for args, ret in asserts:
        assert len(args) <= longest_call
        try:
            for i, arg in enumerate(args):
                arg_types[i].append(get_type_annot(eval(arg)))
            return_types.append(get_type_annot(eval(ret)))
        except Exception as _:
            pass

    # at least one type
    assert len(return_types) > 0
    assert all(len(arg) > 0 for arg in arg_types)

    return (list(map(full_pizza, arg_types)), full_pizza(return_types))


def required_imports(annots: List[Annot]) -> Set[str]:
    imports = set()
    for annot in annots:
        if annot.name in ["List", "Tuple", "Set", "Dict", "Union", "Optional"]:
            imports.add(annot.name)
        if annot.children:
            imports.update(required_imports(list(annot.children)))
    return imports


if __name__ == "__main__":
    import unittest

    class TestInfer(unittest.TestCase):

        def test_get_type_annot(self):
            self.assertEqual(get_type_annot(1), Annot("int"))
            self.assertEqual(get_type_annot(1.0), Annot("float"))
            self.assertEqual(get_type_annot('1'), Annot("str"))
            self.assertEqual(get_type_annot(None), Annot("None"))
            self.assertEqual(get_type_annot([1, 2, 3]), Annot(
                "List", children=[Annot("int")]))
            self.assertEqual(get_type_annot((1, 2, 3)), Annot("Tuple", children=[
                Annot("int"), Annot("int"), Annot("int")]))
            self.assertEqual(get_type_annot({1, 2, 3}), Annot(
                "Set", children=[Annot("int")]))
            self.assertEqual(get_type_annot({1: False, 3: True}), Annot("Dict", children=[
                Annot("int"), Annot("bool")]))
            self.assertEqual(get_type_annot(True), Annot("bool"))
            self.assertEqual(get_type_annot(False), Annot("bool"))
            self.assertEqual(get_type_annot(
                {}), Annot("Dict", improvable=True))
            self.assertEqual(get_type_annot(
                []), Annot("List", improvable=True))
            self.assertEqual(get_type_annot(
                ()), Annot("Tuple", improvable=True))
            self.assertEqual(get_type_annot(set()),
                             Annot("Set", improvable=True))
            # trying out eval
            self.assertEqual(get_type_annot(eval('1')), Annot("int"))
            self.assertEqual(get_type_annot(eval('1.0')), Annot("float"))
            self.assertEqual(get_type_annot(eval('"1"')), Annot("str"))
            self.assertEqual(get_type_annot(eval('None')), Annot("None"))

            # complex union types
            self.assertEqual(get_type_annot([1, 2, '3']), Annot("List", children=[
                Annot("Union", children=set([Annot("int"), Annot("str")]))]))
            self.assertEqual(get_type_annot({1: False, 3: 'True'}), Annot("Dict", children=[
                Annot("int"), Annot("Union", children=set([Annot("bool"), Annot("str")]))]))

        def test_unify_annotations(self):
            self.assertEqual(unify_annotations([Annot("int"), Annot("int")]),
                             Annot("int"))
            self.assertEqual(unify_annotations([Annot("int"), Annot("float")]),
                             Annot("Union", children={Annot("int"), Annot("float")}))
            self.assertEqual(unify_annotations([Annot("int"), Annot("float"), Annot("int")]),
                             Annot("Union", children={Annot("int"), Annot("float")}))
            self.assertEqual(unify_annotations([Annot("int"), Annot("float"), Annot("str")]),
                             Annot("Union", children={Annot("int"), Annot("float"), Annot("str")}))
            self.assertEqual(unify_annotations([Annot("int"), Annot("float"), Annot("str"), Annot("int")]),
                             Annot("Union", children={Annot("int"), Annot("float"), Annot("str")}))
            self.assertEqual(unify_annotations([Annot("int"), Annot("float"), Annot("str"), Annot("int"), Annot("float")]),
                             Annot("Union", children={Annot("int"), Annot("float"), Annot("str")}))
            # unifying unions
            self.assertEqual(unify_annotations([Annot("Union", children=set([Annot("int"), Annot("float")])), Annot("int")]),
                             Annot("Union", children={Annot("int"), Annot("float")}))
            self.assertEqual(unify_annotations([Annot("int"), Annot("Union", children=set([Annot("int"), Annot("float")]))]),
                             Annot("Union", children={Annot("int"), Annot("float")}))
            self.assertEqual(unify_annotations([Annot("Union", children=set([Annot("int"), Annot("float")])), Annot("Union", children=set([Annot("int"), Annot("float")]))]),
                             Annot("Union", children={Annot("int"), Annot("float")}))
            self.assertEqual(unify_annotations([Annot("Union", children=set([Annot("int"), Annot("float")])), Annot("Union", children=set([Annot("int"), Annot("str")]))]),
                             Annot("Union", children={Annot("int"), Annot("float"), Annot("str")}))
            self.assertEqual(unify_annotations([Annot("Union", children=set([Annot("int"), Annot("float")])), Annot("Union", children=set([Annot("int"), Annot("str"), Annot("float")]))]),
                             Annot("Union", children={Annot("int"), Annot("float"), Annot("str")}))
            self.assertEqual(unify_annotations([Annot("Union", children=set([Annot("int"), Annot("float"), Annot("str")])), Annot("Union", children=set([Annot("int"), Annot("str"), Annot("float")]))]),
                             Annot("Union", children={Annot("int"), Annot("float"), Annot("str")}))

            # nested list types
            self.assertEqual(unify_annotations([Annot("List", children=[Annot("int")]), Annot("List", children=[Annot("int")])]),
                             Annot("List", children=[Annot("int")]))
            # [List[int], List[float]] -> List[Union[int, float]]
            self.assertEqual(unify_annotations([Annot("List", children=[Annot("int")]), Annot("List", children=[Annot("float")])]),
                             Annot("List", children=[Annot("Union", children={Annot("int"), Annot("float")})]))
            # [List[int], List[float], List[int]] -> List[Union[int, float]]
            self.assertEqual(unify_annotations([Annot("List", children=[Annot("int")]), Annot("List", children=[Annot("float")]), Annot("List", children=[Annot("int")])]),
                             Annot("List", children=[Annot("Union", children={Annot("int"), Annot("float")})]))
            # [List[int], List[float], List[int], List[float]] -> List[Union[int, float]]
            self.assertEqual(unify_annotations([Annot("List", children=[Annot("int")]), Annot("List", children=[Annot("float")]), Annot("List", children=[Annot("int")]), Annot("List", children=[Annot("float")])]),
                             Annot("List", children=[Annot("Union", children={Annot("int"), Annot("float")})]))
            # [List[int], List[float], List[str], List[int], List[float]] -> List[Union[int, float, str]]
            self.assertEqual(unify_annotations([Annot("List", children=[Annot("int")]), Annot("List", children=[Annot("float")]), Annot("List", children=[Annot("str")]), Annot("List", children=[Annot("int")]), Annot("List", children=[Annot("float")])]),
                             Annot("List", children=[Annot("Union", children={Annot("int"), Annot("float"), Annot("str")})]))
            # [List[List[int]], List[List[float]]] -> List[List[Union[int, float]]]
            self.assertEqual(unify_annotations([Annot("List", children=[Annot("List", children=[Annot("int")])]), Annot("List", children=[Annot("List", children=[Annot("float")])])]),
                             Annot("List", children=[Annot("List", children=[Annot("Union", children={Annot("int"), Annot("float")})])]))

            # [List[Union[int, float]], List[str]] -> List[Union[int, float, str]]
            self.assertEqual(unify_annotations([Annot("List", children=[Annot("Union", children={Annot("int"), Annot("float")})]), Annot("List", children=[Annot("str")])]),
                             Annot("List", children=[Annot("Union", children={Annot("int"), Annot("float"), Annot("str")})]))

            # tuples
            self.assertEqual(unify_annotations([Annot("Tuple", children=[Annot("int")]), Annot("Tuple", children=[Annot("int")])]),
                             Annot("Tuple", children=[Annot("int")]))
            self.assertEqual(unify_annotations([Annot("Tuple", children=[Annot("int"), Annot("float")]), Annot("Tuple", children=[Annot("int"), Annot("float")])]),
                             Annot("Tuple", children=[Annot("int"), Annot("float")]))
            self.assertEqual(unify_annotations([Annot("Tuple", children=[Annot("int"), Annot("float")]), Annot("Tuple", children=[Annot("int"), Annot("str")])]),
                             Annot("Tuple", children=[Annot("int"), Annot("Union", children={Annot("float"), Annot("str")})]))
            self.assertEqual(unify_annotations([Annot("Tuple", children=[Annot("int"), Annot("float")]), Annot("Tuple", children=[Annot("int"), Annot("str"), Annot("float")])]),
                             Annot("Union", children={Annot("Tuple", children=[Annot("int"), Annot("float")]), Annot("Tuple", children=[Annot("int"), Annot("str"), Annot("float")])}))

            # dicts
            self.assertEqual(unify_annotations([Annot("Dict", children=[Annot("int"), Annot("int")]), Annot("Dict", children=[Annot("int"), Annot("int")])]),
                             Annot("Dict", children=[Annot("int"), Annot("int")]))
            self.assertEqual(unify_annotations([Annot("Dict", children=[Annot("int"), Annot("int")]), Annot("Dict", children=[Annot("int"), Annot("float")])]),
                             Annot("Dict", children=[Annot("int"), Annot("Union", children={Annot("int"), Annot("float")})]))
    unittest.main()
