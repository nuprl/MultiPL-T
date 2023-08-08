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

(* small-list? : does the list contain fewer than 3 elements? *)
let is_small_list (lox: 'a list) : bool = (List.length lox) < 2;;


(* small-group? : does the event have fewer than 3 participants? *)
let is_small_group (e: event) : bool =
  match e with 
  | Call(_, (_, att, _)) -> is_small_list att
  | Mtg(_, att, _) -> is_small_list att
  | Alone(_, _) -> true
;;

(* small-group : produces the list of only those events that have up to 3 participants
(including the implied author). *)
let small_groups (events: event list) : event list = List.filter is_small_group events;;

(* <tests> *)
let ex_zoom_doc = Call(Zoom, (22, ["Dr. Zoidberg"], "Doctor appointment"));;
let ex_teams_office = Call(Teams, (7, ["Mike"; "Tajel"], "Office hours"));;
let ex_phone_spam = Call(Phone, (1, ["Unknown"], "Spam"));;

let ex_mtg_study = Mtg(62, ["Rachel"; 
                            "Ross"; 
                            "Joey"; 
                            "Phoebe"; 
                            "Chandler"; 
                            "Monica"], "Study group");;
let ex_mtg_advisor = Mtg(28, ["Ali"], "Research meeting");;

let ex_alone_lunch = Alone(34, "lunch");;
let ex_alone_reading = Alone(25, "Reading Infinite Jest");;

let assertions () = 
  assert (small_groups [] = []);
  assert (small_groups [ex_zoom_doc; 
                        ex_teams_office; 
                        ex_phone_spam; 
                        ex_mtg_study; 
                        ex_mtg_advisor;
                        ex_alone_lunch;
                        ex_alone_reading] = 
        [ex_zoom_doc; 
        ex_phone_spam; 
        ex_mtg_advisor; 
        ex_alone_lunch; 
        ex_alone_reading])
;;

assertions()