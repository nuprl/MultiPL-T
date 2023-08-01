(* A Phone is one of:
- iphone [num_version]
- android [str_version]
- pixel [num_version]
Interpretation: A type of smartphone *)
type phone = 
    | Iphone of int
    | Android of string
    | Pixel of int
;;

(* apply_to_android:
Applies a func to the androids in the list of phones *)
let rec apply_to_android (f: 'a -> 'b) (p: phone list) : phone list = 
    match p with
    | [] -> []
    | Android(s) :: xs -> f(Android(s)) :: apply_to_android f xs
    | x :: xs -> x :: apply_to_android f xs
  ;;
  
(* <tests> *)
let uppercase_android (p : phone) : phone = 
    match p with
    | Android(s) -> Android(String.uppercase_ascii(s))
    | _ -> p
  ;;

let reverse_android (p : phone) : phone = 
    match p with
    | Android(s) -> Android(String.concat "" ( List.rev (Str.split (Str.regexp "") "Cupcake")))
    | _ -> p
  ;;

let version_android (p : phone) : phone = 
    match p with
    | Android(s) -> Android(s ^ "-v2")
    | _ -> p
  ;;

let assertions () = 
    assert (apply_to_android uppercase_android [] = []);
    assert (apply_to_android uppercase_android 
        [Iphone(5); Pixel(3); Android("Cupcake"); Pixel(7); Android("Grape"); Iphone(10); Android("Orange")] = 
        [Iphone(5); Pixel(3); Android("CUPCAKE"); Pixel(7); Android("GRAPE"); Iphone(10); Android("ORANGE")]);
    assert (apply_to_android reverse_android
        [Iphone(5); Pixel(3); Android("Cupcake")] = 
        [Iphone(5); Pixel(3); Android("ekacpuC")]);
    assert (apply_to_android version_android
        [Iphone(5); Pixel(3)] = 
        [Iphone(5); Pixel(3)])
  ;;

assertions()
