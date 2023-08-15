(* hello : String -> String
Greets you hello! *)
let hello s =
  "Hello " ^ s ^ "!"

(* goodbye : String -> String 
Greets you goodbye! *)
let goodbye s =
  "Goodbye " ^ s ^ "!"

(* hello_goodbye : [List-of String] -> [List-of String]
Greets everyone hello and goodbye. *)
let rec hello_goodbye (l : string list) : string list =
  double_do_it hello goodbye l

(* double_do_it : helper for hello_goodbye. Applies both functions to each item in the list 
and returns a new list with the output of both functions. *)
and double_do_it (f : 'a -> 'a) (g : 'a -> 'a) (l : 'a list) : 'a list =
  (* <solution> *)
  match l with
  | [] -> []
  | x :: xs -> f x :: g x :: double_do_it f g xs
;;

(* <tests> *)
let assertions() =
  assert (hello_goodbye [] = []);
  let result = hello_goodbye ["Alice"; "Bob"] in
  let expected = ["Hello Alice!"; "Goodbye Alice!"; "Hello Bob!"; "Goodbye Bob!"] in
  assert (result = expected);
  let result = double_do_it goodbye hello ["Alice"; "Bob"] in
  let expected = ["Goodbye Alice!"; "Hello Alice!"; "Goodbye Bob!"; "Hello Bob!"] in
  assert (result = expected);
  (* let result = double_do_it (fun x -> x * x) sqrt [25; 9] in
  let expected = [625.0; 5.0; 81.0; 3.0] in
  assert (result = expected); *)
;;

assertions();
