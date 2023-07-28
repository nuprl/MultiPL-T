(* A CallType is one of:
- "zoom"
- "teams"
- "phone"
Interpretation: a type of call *)
type call = 
  | Zoom 
  | Teams 
  | Phone
;;

(* Duration attendees description *)
type dur_att_desc = int * string list * string ;;

(* An Event is one of:
- call [call-type duration]
- meeting [duration]
- alone time [num_minutes description]
Interpretation: an event in some period of time, which is either:
- A call using some technology, lasting some number of minutes with attendees
 (by name), and a description;
- An in-person meeting lasting some number of minutes
  with attendees (by name) and a description; or
- Time spent alone for some number of minutes with a description. *)
type event = 
  | Call of call * dur_att_desc
  | Mtg of dur_att_desc
  | Alone of int * string
;;

(* weekend : produces a list of 30min rest alone events of a supplied size *)
let rec weekend (n: int): event list =
  match n with
    | 0 -> []
    | _ -> Alone(30, "rest") :: weekend (n-1)
;;

(* <tests> *)
let assertions () = 
  assert (weekend 1 = [Alone(30, "rest")]);
  assert (weekend 3 = [Alone(30, "rest"); Alone(30, "rest"); Alone(30, "rest")]);;

assertions()
