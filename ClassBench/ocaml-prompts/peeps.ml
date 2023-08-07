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

(* participant_names : gets the list of participants from an event *)
let participant_names (e: event) : string list = 
  match e with 
  | Call(_, (_, a, _)) -> a
  | Mtg(_, a, _) -> a
  | Alone(_, _) -> []
;;

(* events_names : produces a single list of the names of all attendees *)
let event_names (loe: event list) : string list = List.fold_left List.append [] (List.map participant_names loe);;


(* peeps : alphabetized list from all event attendees *)
let peeps (loe: event list) : string list = 
  List.sort compare (event_names loe)
;;

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
  assert (peeps [] = []);
  assert (peeps [ex_teams_office] = ["Mike"; "Tajel"]);
  assert (peeps [ex_zoom_doc; 
                        ex_teams_office; 
                        ex_phone_spam; 
                        ex_mtg_study; 
                        ex_mtg_advisor;
                        ex_alone_lunch;
                        ex_alone_reading] = ["Ali"; 
                                             "Chandler"; 
                                             "Dr. Zoidberg"; 
                                             "Joey"; 
                                             "Mike"; 
                                             "Monica"; 
                                             "Phoebe"; 
                                             "Rachel"; 
                                             "Ross"; 
                                             "Tajel"; 
                                             "Unknown"])
;;

assertions ()