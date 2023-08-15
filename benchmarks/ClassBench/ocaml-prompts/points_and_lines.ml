(* A Point is:
 * - Point of int * int
 * Interpretation: A point on a cartesian plane *)
 type point = { x : int; y : int }

 (* A Line is:
  * - Line of point * point
  * Interpretation: A straight line between two points *)
 type line = { start : point; end_ : point }
 
 (* manhattan_distance: line -> int
  * Computes the manhattan distance that a line covers
  * manhattan distance = |x_1 - x_2| + |y_1 - y_2| *)
 let manhattan_distance (l : line) : int =
   (* <solution> *)
   let dx = abs (l.start.x - l.end_.x) in
   let dy = abs (l.start.y - l.end_.y) in
   dx + dy
 
 (* <tests> *)
 let assertions() =
   assert (manhattan_distance { start = { x = 0; y = 0 }; end_ = { x = 4; y = 3 } } = 7);
   assert (manhattan_distance { start = { x = 1; y = 3 }; end_ = { x = 4; y = 3 } } = 3);
   assert (manhattan_distance { start = { x = 4; y = 2 }; end_ = { x = 4; y = 3 } } = 1);
   assert (manhattan_distance { start = { x = 5; y = 8 }; end_ = { x = 4; y = 3 } } = 6);
   assert (manhattan_distance { start = { x = 7; y = 0 }; end_ = { x = 7; y = -5 } } = 5);
   assert (manhattan_distance { start = { x = 4; y = 0 }; end_ = { x = -4; y = 0 } } = 8)
 ;;

assertions();;