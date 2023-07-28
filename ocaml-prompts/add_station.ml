(* A Station type consists of the name of a station and the names of stations 
that directly connect to it. *)
type station = 
  | Station of string * (string list) 

(* A Subway is a list of stations that make a subway network. *)

(* has_connection: checks if a station has a connection to some other station name. *)
let has_connection (str: string) (stat: station) : bool = 
  let Station(_, connections) = stat 
    in List.exists (fun t -> String.equal t str) connections
;;

(* add_edge_to_station: Adds a single edge to a station. *)
let add_edge_to_station (str: string) (stat: station) : station = 
    if (has_connection str stat) 
    then stat 
    else 
      let Station(name, connections) = stat in Station(name, str::connections)
;;

(* add_edge : adds a connection to the subway, if it does not
already exist. Assumes that both from and to stations in the Subway. *)
let rec add_edge (from: string) (towards: string) (sub: station list) : station list = 
  match sub with 
  | [] -> sub 
  | x::xs -> let Station(name, _) = x 
             in if String.equal from name 
                then (add_edge_to_station towards x) :: xs
                else x :: (add_edge from towards xs)
;;

(* <tests> *)
let ex_station_1 = Station("Newton Centre", ["Fenway"; "Kenmore"]) ;;
let ex_station_2 = Station("Fenway", ["Newton Highlands"; "Newton Centre"]);; 
let ex_station_3 = Station("Kenmore", ["Newton Centre"]);;
let ex_station_4 = Station("Newton Highlands", ["Fenway"]);;

let ex_subway_1 = [ex_station_1; ex_station_2; ex_station_3; ex_station_4];;
let ex_subway_2 = [];;
let ex_subway_3 = [
  Station("A", ["B"; "D"]);
  Station("B", ["C"; "A"]);
  Station("C", ["D"; "B"]);
  Station("D", ["A"; "C"]);
];;

let assertions () = 
  assert (add_edge "Fenway" "Kenmore" ex_subway_2 = ex_subway_2);
  assert (add_edge "Fenway" "Kenmore" ex_subway_1 = [ 
    ex_station_1;
    Station("Fenway", ["Kenmore"; "Newton Highlands"; "Newton Centre"]);
    ex_station_3;
    ex_station_4;
  ]);
  assert (add_edge "Kenmore" "Newton Centre" ex_subway_1 = ex_subway_1);
  assert (add_edge "A" "C" ex_subway_3 = [ 
    Station("A", ["C"; "B"; "D"]);
    Station("B", ["C"; "A"]);
    Station("C", ["D"; "B"]);
    Station("D", ["A"; "C"])
  ])
;;

assertions();;