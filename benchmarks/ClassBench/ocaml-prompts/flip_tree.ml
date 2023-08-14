(* A NumTree is one of:
- Number NumTree NumTree
- Number
Interpretation: A set of numbers arranged in a tree shape. *)
type numtree =
  | Leaf of int
  | Node of int * numtree * numtree

(* mirror: NumTree -> NumTree
Mirrors the tree around the center point *)
let rec mirror tree =
  (* <solution> *)
  match tree with
  | Leaf num -> Leaf num
  | Node (num, left, right) -> Node (num, mirror right, mirror left)

(* <tests> *)
let assertions() =
  assert (mirror (Leaf 5) = Leaf 5);
  assert (mirror (Node (5, Leaf 6, Leaf 7)) = Node (5, Leaf 7, Leaf 6));
  assert (mirror (Node (5, Node (8, Leaf 9, Leaf 10), Node (4, Leaf 3, Leaf 2))) =
          Node (5, Node (4, Leaf 2, Leaf 3), Node (8, Leaf 10, Leaf 9)))
;;

assertions();;
