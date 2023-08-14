(* A Shape is one of:
 * - Circle of float
 * - Rectangle of float * float
 * - Triangle of float * float * float *)
 type shape =
 | Circle of float
 | Rectangle of float * float
 | Triangle of float * float * float

(* circle_perimeter: shape -> float *)
(* Computes the perimeter of a circle *)
let circle_perimeter c =
 6.28 *. c

(* rectangle_perimeter: shape -> float *)
(* Computes the perimeter of a rectangle *)
let rectangle_perimeter r =
 2.0 *. (fst r) +. 2.0 *. (snd r)

(* triangle_perimeter: shape -> float *)
(* Computes the perimeter of a triangle *)
let triangle_perimeter t =
 let (s1, s2, s3) = t in
 s1 +. s2 +. s3

(* shape_perimeter: shape -> float *)
(* Computes the perimeter of any Shape *)
let shape_perimeter s =
 (* <solution> *)
 match s with
 | Circle r -> circle_perimeter r
 | Rectangle (s1, s2) -> rectangle_perimeter (s1, s2)
 | Triangle (s1,s2,s3) -> triangle_perimeter (s1, s2, s3)

(* <tests> *)
let assertions() =
 assert (shape_perimeter (Circle 2.0) = 12.56);
 assert (shape_perimeter (Rectangle (2.0, 4.0)) = 12.0);
 assert (shape_perimeter (Triangle (3.0, 3.0, 3.0)) = 9.0)
;;

assertions();;