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

(* has_lunch? : does this contain the word "lunch"? *)
let has_lunch (s: string) : bool = 
  let r = Str.regexp(".*lunch.*") in Str.string_match r s 0;; 

(* is_lunch? : is this a lunch event? *)
let is_lunch (e: event) : bool = 
  match e with
  | Call(_, (_, _, desc)) -> has_lunch desc
  | Mtg(_, _, desc) -> has_lunch desc
  | Alone(_, desc) -> has_lunch desc
;;

(* had-lunch? : Is there an event containing the word lunch? *)
let had_lunch (loe : event list) : bool = List.exists is_lunch loe;;

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
  assert (had_lunch [] = false);
  assert (had_lunch [ex_zoom_doc; 
                        ex_teams_office; 
                        ex_phone_spam; 
                        ex_mtg_study; 
                        ex_mtg_advisor;
                        ex_alone_lunch;
                        ex_alone_reading] = true);
  assert (had_lunch [ex_zoom_doc] = false);
  assert (had_lunch [ex_zoom_doc; (Call(Zoom, (20, ["Mom"], "virtual lunch")))] = true);;

assertions();;