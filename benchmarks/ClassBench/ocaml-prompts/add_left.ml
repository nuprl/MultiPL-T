(* A NumTree is one of:
- Number NumTree NumTree
- Number
Interpretation: A set of numbers arranged in a tree shape. *)
type numtree =
  | Leaf of int
  | Node of int * numtree * numtree

(* add_left: NumTree -> Number
Adds only the numbers on the leftmost side of the tree. *)
let rec add_left (tree : numtree) : int =
  (* <solution> *)
  match tree with
  | Leaf num -> num
  | Node (num, left, right) -> num + add_left left

(* <tests> *)
let assertions() =
  assert (add_left (Leaf 4) = 4);
  assert (add_left (Node (5, Leaf 6, Leaf 7)) = 11);
  assert (add_left (Node (-3, Node (3, Leaf 0, Leaf 9), Node (4, Leaf 9, Leaf 10))) = 0)
;;

assertions();;