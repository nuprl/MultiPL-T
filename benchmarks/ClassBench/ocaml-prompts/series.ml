(* A Computable is:
- computable ['a ('a -> 'a)]
Interpretation: A value that can be processed by the function to make a series *)
type 'a computable = 
    | Computable of 'a * ('a -> 'a)
;;

(* update: Runs the computable to make the next computable in the series *)
let update (c: 'a computable) : 'a computable =
    (* <solution> *)
    match c with
    | Computable(value, func) -> Computable(func value, func)
;;

(* <tests> *)

let match_computable_value (comp1 : 'a computable)(comp2 : 'a computable) : bool =
    (* assumes funcs are the same *)
    match (comp1, comp2) with
    | (Computable(value1, _), Computable(value2, _)) -> value1 = value2
;;
    
let assertions() =
    assert (match_computable_value(update(Computable(5, (fun x -> x + 1))), Computable(6, (fun x -> x + 1)));
    (* assert (update (Computable(2, (fun x -> x * x))) = Computable(4, (fun x -> x * x)));
    assert (update (Computable(6, (fun x -> x / 2))) = Computable(3, (fun x -> x / 2))); *)
;;

assertions()