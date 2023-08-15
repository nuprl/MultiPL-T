(* A Computable is:
- Computable of (X * (X -> X))
Interpretation: A value that can be processed by the function to make a series *)
type 'a computable = Computable of 'a * ('a -> 'a)

(* update: 'a computable -> 'a computable *)
(* Runs the computable to make the next computable in the series *)
let update (c : 'a computable) : 'a computable =
  (* <solution> *)
  match c with
  | Computable (value, func) -> Computable (func value, func)

(* <tests> *)
let add_one x = x + 1
let zero_add_one = Computable (5, add_one)
let one_add_one = update zero_add_one
let two_add_one = update one_add_one
let repeat_string s = s ^ s
let zero_repeat_string = Computable ("hi", repeat_string)
let one_repeat_string = update zero_repeat_string
let two_repeat_string = update one_repeat_string
let divide_2 x = x / 2
let zero_divide_2 = Computable (40, divide_2)
let one_divide_2 = update zero_divide_2
let two_divide_2 = update one_divide_2

let assertions() =
  assert (let Computable (value, _) = update zero_add_one in value = 6);
  assert (let Computable (value, _) = update one_add_one in value = 7);
  assert (let Computable (value, _) = update two_add_one in value = 8);
  assert (let Computable (value, _) = update zero_repeat_string in value = "hihi");
  assert (let Computable (value, _) = update one_repeat_string in value = "hihihihi");
  assert (let Computable (value, _) = update two_repeat_string in value = "hihihihihihihihi");
  assert (let Computable (value, _) = update zero_divide_2 in value = 20);
  assert (let Computable (value, _) = update one_divide_2 in value = 10);
  assert (let Computable (value, _) = update two_divide_2 in value = 5)
;;

assertions();;
