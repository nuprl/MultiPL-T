(* Collatz: Number ->  Number
Counts how many steps it takes a number to converge
to 1 through the collatz sequence. The collatz sequence
divides by 2 if the number is even, otherwise if the number
is odd, it multiplies by 3 and adds 1 *)
let rec collatz num =
  (* <solution> *)
  if num = 1 then
    0
  else
    1 + collatz (if num mod 2 = 1 then num * 3 + 1 else num / 2)

(* <tests> *)
let assertions() =
  assert (collatz 1 = 0);
  assert (collatz 2 = 1);
  assert (collatz 4 = 2);
  assert (collatz 3 = 7);
  assert (collatz 12 = 9)
;;

assertions();;