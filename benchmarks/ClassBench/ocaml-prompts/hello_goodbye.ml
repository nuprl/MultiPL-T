(* hello : string -> string *)
(* Greets you hello! *)
let hello s =
  "Hello " ^ s ^ "!"

(* goodbye : string -> string *)
(* Greets you goodbye! *)
let goodbye s =
  "Goodbye " ^ s ^ "!"

(* hello_goodbye : string list -> string list *)
(* Greets everyone hello and goodbye. *)
let hello_goodbye l =
  double_do_it hello goodbye l

(* double_do_it : ('a -> 'b) ('a -> 'b) 'a list -> 'b list *)
(* Helper for hello_goodbye. Applies both functions to each item in the list 
   and returns a new list with the output of both functions. *)
let rec double_do_it f g l =
  match l with
  | [] -> []
  | x :: xs -> f x :: g x :: double_do_it f g xs

(* <tests> *)
let assertions() =
  assert (hello_goodbye [] = []);
  assert (hello_goodbye ["Alice"; "Bob"] =
          ["Hello Alice!"; "Goodbye Alice!"; "Hello Bob!"; "Goodbye Bob!"]);
  assert (double_do_it (fun x -> x * x) (fun x -> sqrt (float_of_int x)) [] = []);
  assert (double_do_it (fun x -> x * x) (fun x -> sqrt (float_of_int x)) [25; 9] =
          [625; 5.; 81; 3.])
;;

assertions();;

(* hello : Greets you hello!
let hello (s : string) : string =
  "Hello " ^ s ^ "!"
;;

(* goodbye : Greets you goodbye! *)
let goodbye (s : string) : string =
  "Goodbye " ^ s ^ "!"
;; *)

(* hello-goodbye : Greets everyone hello and goodbye.
let hello_goodbye (l : string list) : string list =
  double_do_it hello goodbye l
;; *)

(* double-do-it : (X Y) [X -> Y] [X -> Y] [List-of X] -> [List-of Y]
Helper for hello-goodbye. *)
(* let double_do_it f g l 
  (cond
    [(empty? l) '()]
    [(cons? l)
     (cons (f (first l))
           (cons (g (first l))
                 (double-do-it f g (rest l))))]))
                  *)
(* <tests> *)
(* (check-equal? (hello-goodbye '()) '())
(check-equal?
 (hello-goodbye (cons "Alice" (cons "Bob" '())))
 (cons "Hello Alice!" (cons "Goodbye Alice!" (cons "Hello Bob!" (cons "Goodbye Bob!" '())))))
(check-equal? (double-do-it sqr sqrt '()) '())
(check-equal? (double-do-it sqr sqrt (cons 25 (cons 9 '())))
              (cons 625 (cons 5 (cons 81 (cons 3 '()))))) *)