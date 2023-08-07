(* A Station is a [string (string list)]
Interpretation: The name of a station and the names of stations that directly
connect to it. *)
type station = 
  | Station of string * (string list) 

(* A Subway is a [station list]
Interpretation: A list of stations that make a subway network.

A [List-of-2-or-more X] is one of:
- x :: x :: []
- (x :: xs) where xs is a [List-of-2-or-more X]
A list with two or more items. *)

(* has_connection : checks if a station has a connection. *)
let has_connection (str: string) (stat: station) : bool = 
  let Station(_, connections) = stat 
    in List.exists (fun t -> String.equal t str) connections
;;

(* add_edge_to_station : adds a connection to the station, if it does not
already exist. *)
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

(* add_forward_edges : Subway [List-of-2-or-more String] -> Subway
Adds the list of stations to the subway, in one direction. *)
let rec add_forward_edges (subway : station list) (lo2om : string list) : station list = 
  match lo2om with 
  | [] -> subway
  | x::[] -> subway 
  | x::y::[] -> add_edge x y subway
  | x::y::xs -> add_forward_edges (add_edge x y subway) (y::xs)
;;

(* line_to_subway: Given a list of stations on a line, produces a Subway. *)
let line_to_subway (line: string list) : station list = 
  let disconnected_stations = List.map (fun n -> Station(n, [])) line in 
  let forward_connections_added = add_forward_edges disconnected_stations line in 
  let backward_connections_added = add_forward_edges forward_connections_added (List.rev line) in 
  backward_connections_added

(* <tests> *)
let assertions() =
  assert (line_to_subway ["A"; "B"] = [
    Station("A", ["B"]);
    Station("B", ["A"]);
  ]);
  assert (line_to_subway ["A"; "B"; "C"] = [
    Station("A", ["B"]);
    Station("B", ["A"; "C"]);
    Station("C", ["B"]);
  ]);;

assertions()