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

(* social-time : gets the duration of calls and meetings; 0 for alone *)
let social_event_time (e : event) : int = 
  match e with 
  | Call(_, (t, _, _)) -> t
  | Mtg (t, _, _) -> t
  | Alone(_) -> 0
;;

(* social-time : how much time was spent on calls and meetings? *)
let social_time (events: event list) : int = 
  List.fold_right (+) (List.map social_event_time events) 0;;

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
  assert (social_time [] = 0);
  assert (social_time [ex_zoom_doc; 
                        ex_teams_office; 
                        ex_phone_spam; 
                        ex_mtg_study; 
                        ex_mtg_advisor;
                        ex_alone_lunch;
                        ex_alone_reading] = 120)
;;

assertions()