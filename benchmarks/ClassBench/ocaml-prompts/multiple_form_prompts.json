[
  {
    "name": "series",
    "language": "ocaml",
    "prompt": "(* A Computable is:\n- Computable of (X * (X -> X))\nInterpretation: A value that can be processed by the function to make a series *)\ntype 'a computable = Computable of 'a * ('a -> 'a)\n\n(* update: 'a computable -> 'a computable *)\n(* Runs the computable to make the next computable in the series *)\nlet update (c : 'a computable) : 'a computable =",
    "doctests": "original",
    "original": "ocaml-prompts/series.ml",
    "prompt_terminology": "verbatim",
    "tests": "let add_one x = x + 1\nlet zero_add_one = Computable (5, add_one)\nlet one_add_one = update zero_add_one\nlet two_add_one = update one_add_one\nlet repeat_string s = s ^ s\nlet zero_repeat_string = Computable (\"hi\", repeat_string)\nlet one_repeat_string = update zero_repeat_string\nlet two_repeat_string = update one_repeat_string\nlet divide_2 x = x / 2\nlet zero_divide_2 = Computable (40, divide_2)\nlet one_divide_2 = update zero_divide_2\nlet two_divide_2 = update one_divide_2\n\nlet assertions() =\n  assert (let Computable (value, _) = update zero_add_one in value = 6);\n  assert (let Computable (value, _) = update one_add_one in value = 7);\n  assert (let Computable (value, _) = update two_add_one in value = 8);\n  assert (let Computable (value, _) = update zero_repeat_string in value = \"hihi\");\n  assert (let Computable (value, _) = update one_repeat_string in value = \"hihihihi\");\n  assert (let Computable (value, _) = update two_repeat_string in value = \"hihihihihihihihi\");\n  assert (let Computable (value, _) = update zero_divide_2 in value = 20);\n  assert (let Computable (value, _) = update one_divide_2 in value = 10);\n  assert (let Computable (value, _) = update two_divide_2 in value = 5)\n;;\n\nassertions();;",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "add_left",
    "language": "ocaml",
    "prompt": "(* A NumTree is one of:\n- Number NumTree NumTree\n- Number\nInterpretation: A set of numbers arranged in a tree shape. *)\ntype numtree =\n  | Leaf of int\n  | Node of int * numtree * numtree\n\n(* add_left: NumTree -> Number\nAdds only the numbers on the leftmost side of the tree. *)\nlet rec add_left (tree : numtree) : int =",
    "doctests": "original",
    "original": "ocaml-prompts/add_left.ml",
    "prompt_terminology": "verbatim",
    "tests": "let assertions() =\n  assert (add_left (Leaf 4) = 4);\n  assert (add_left (Node (5, Leaf 6, Leaf 7)) = 11);\n  assert (add_left (Node (-3, Node (3, Leaf 0, Leaf 9), Node (4, Leaf 9, Leaf 10))) = 0)\n;;\n\nassertions();;",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "hello_goodbye",
    "language": "ocaml",
    "prompt": "(* hello : String -> String\nGreets you hello! *)\nlet hello (s : string) : string =\n  \"Hello \" ^ s ^ \"!\"\n\n(* goodbye : String -> String \nGreets you goodbye! *)\nlet goodbye (s : string) : string =\n  \"Goodbye \" ^ s ^ \"!\"\n\n(* hello_goodbye : [List-of String] -> [List-of String]\nGreets everyone hello and goodbye. *)\nlet rec hello_goodbye (l : string list) : string list =\n  double_do_it hello goodbye l\n\n(* double_do_it : helper for hello_goodbye. Applies both functions to each item in the list \nand returns a new list with the output of both functions. *)\nand double_do_it (f : 'a -> 'a) (g : 'a -> 'a) (l : 'a list) : 'a list =",
    "doctests": "original",
    "original": "ocaml-prompts/hello_goodbye.ml",
    "prompt_terminology": "verbatim",
    "tests": "let assertions() =\n  assert (hello_goodbye [] = []);\n  let result = hello_goodbye [\"Alice\"; \"Bob\"] in\n  let expected = [\"Hello Alice!\"; \"Goodbye Alice!\"; \"Hello Bob!\"; \"Goodbye Bob!\"] in\n  assert (result = expected);\n  let result = double_do_it goodbye hello [\"Alice\"; \"Bob\"] in\n  let expected = [\"Goodbye Alice!\"; \"Hello Alice!\"; \"Goodbye Bob!\"; \"Hello Bob!\"] in\n  assert (result = expected);\n  (* let result = double_do_it (fun x -> x * x) sqrt [25; 9] in\n  let expected = [625.0; 5.0; 81.0; 3.0] in\n  assert (result = expected); *)\n;;\n\nassertions();\n",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "points_and_lines",
    "language": "ocaml",
    "prompt": "(* A Point is:\n * - Point of int * int\n * Interpretation: A point on a cartesian plane *)\n type point = { x : int; y : int }\n\n (* A Line is:\n  * - Line of point * point\n  * Interpretation: A straight line between two points *)\n type line = { start : point; end_ : point }\n \n (* manhattan_distance: line -> int\n  * Computes the manhattan distance that a line covers\n  * manhattan distance = |x_1 - x_2| + |y_1 - y_2| *)\n let manhattan_distance (l : line) : int =",
    "doctests": "original",
    "original": "ocaml-prompts/points_and_lines.ml",
    "prompt_terminology": "verbatim",
    "tests": " let assertions() =\n   assert (manhattan_distance { start = { x = 0; y = 0 }; end_ = { x = 4; y = 3 } } = 7);\n   assert (manhattan_distance { start = { x = 1; y = 3 }; end_ = { x = 4; y = 3 } } = 3);\n   assert (manhattan_distance { start = { x = 4; y = 2 }; end_ = { x = 4; y = 3 } } = 1);\n   assert (manhattan_distance { start = { x = 5; y = 8 }; end_ = { x = 4; y = 3 } } = 6);\n   assert (manhattan_distance { start = { x = 7; y = 0 }; end_ = { x = 7; y = -5 } } = 5);\n   assert (manhattan_distance { start = { x = 4; y = 0 }; end_ = { x = -4; y = 0 } } = 8)\n ;;\n\nassertions();;",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "band_electrify",
    "language": "ocaml",
    "prompt": "(* A Guitar is a [string string bool]\nInterpretation: A [brand_name color electric] represents a guitar where:\n- brand_name is the brand name of the guitar\n- color is the color of the guitar\n- electric? is a boolean indicating whether the guitar is electric *)\ntype guitar =\n  | Guitar of string * string * bool\n\n(* A DrumKit is a [string bool]\nInterpretation: A [brand_name electric?] represents\na drum kit where:\n- brand_name is the brand name of the drum kit\n- electric? is a boolean indicating whether the drum kit is electric *)\ntype drum_kit =\n  | DrumKit of string * bool\n\n(* A Saxophone is a [string]\nInterpretation: A [brand_name] represents a saxophone where:\n- brand_name is the brand name of the saxophone *)\ntype saxophone =\n  | Saxophone of string\n\n(* A Piano is a [string]\nInterpretation: A [brand_name] represents a piano where:\n- brand_name is the brand name of the piano *)\ntype piano =\n  | Piano of string\n\n(* An Instrument is one of:\n- Guitar\n- DrumKit\n- Saxophone\n- Piano\nInterpretation: An Instrument is one of the four musical instruments. *)\ntype instrument =\n  | Guitar of guitar\n  | DrumKit of drum_kit\n  | Saxophone of saxophone\n  | Piano of piano\n\n(* A Band is one of:\nA Band is one of:\n- OnePieceBand instrument\n- TwoPieceBand instrument instrument\n- ThreePieceBand instrument instrument instrument\nInterpretation: A Band represents a band with one, two, or three members. *)\ntype band =\n  | OnePieceBand of instrument\n  | TwoPieceBand of instrument * instrument\n  | ThreePieceBand of instrument * instrument * instrument\n\n(* electrify-guitar : Makes this an electric guitar! *)\nlet electrify_guitar (g : guitar) : guitar =\n  match g with\n  | Guitar (brand_name, color, _) -> Guitar (brand_name, color, true)\n;;\n\n(* electrify-drum-kit : Makes this an electric drum kit! *)\nlet electrify_drum_kit (d : drum_kit) : drum_kit =\n  match d with\n  | DrumKit (brand_name, _) -> DrumKit (brand_name, true)\n;;\n\n(* electrify-instrument : Makes this an electric instrument! *)\nlet electrify_instrument (i : instrument) : instrument =\n  match i with\n  | Guitar g -> Guitar (electrify_guitar g)\n  | DrumKit d -> DrumKit (electrify_drum_kit d)\n  | _ -> i\n;;\n\n(* electrify-band : Makes this an electric band! *)\nlet electrify_band (b : band) : band =",
    "doctests": "original",
    "original": "ocaml-prompts/band_electrify.ml",
    "prompt_terminology": "verbatim",
    "tests": "let assertions () =\n  assert ((electrify_band (OnePieceBand (Guitar (Guitar (\"Stratocaster\", \"red\", true))))) = \n        OnePieceBand (Guitar (Guitar (\"Stratocaster\", \"red\", true))));\n  assert ((electrify_band (TwoPieceBand (DrumKit (DrumKit (\"Korg\", true)), DrumKit (DrumKit (\"Korg\", false))))) = \n        TwoPieceBand (DrumKit (DrumKit (\"Korg\", true)), DrumKit (DrumKit (\"Korg\", true))));\n  assert ((electrify_band (ThreePieceBand (Guitar (Guitar (\"Stratocaster\", \"red\", true)), DrumKit (DrumKit (\"Korg\", false)), Saxophone (Saxophone (\"Yamaha\"))))) = \n        ThreePieceBand (Guitar (Guitar (\"Stratocaster\", \"red\", true)), DrumKit (DrumKit (\"Korg\", true)), Saxophone (Saxophone (\"Yamaha\"))));\n;;\n\nassertions()\n",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "social_time",
    "language": "ocaml",
    "prompt": "(* A CallType is one of:\n- \"zoom\"\n- \"teams\"\n- \"phone\"\nInterpretation: a type of call *)\ntype call = \n  | Zoom \n  | Teams \n  | Phone\n;;\n\n(* Duration attendees description *)\ntype dur_att_desc = int * string list * string ;;\n\n(* An Event is one of:\n- call [call-type duration]\n- meeting [duration]\n- alone time [num_minutes description]\nInterpretation: an event in some period of time, which is either:\n- A call using some technology, lasting some number of minutes with attendees\n (by name), and a description;\n- An in-person meeting lasting some number of minutes\n  with attendees (by name) and a description; or\n- Time spent alone for some number of minutes with a description. *)\ntype event = \n  | Call of call * dur_att_desc\n  | Mtg of dur_att_desc\n  | Alone of int * string\n;;\n\n(* social-time : gets the duration of calls and meetings; 0 for alone *)\nlet social_event_time (e : event) : int = \n  match e with \n  | Call(_, (t, _, _)) -> t\n  | Mtg (t, _, _) -> t\n  | Alone(_) -> 0\n;;\n\n(* social-time : how much time was spent on calls and meetings? *)\nlet social_time (events: event list) : int = ",
    "doctests": "original",
    "original": "ocaml-prompts/social_time.ml",
    "prompt_terminology": "verbatim",
    "tests": "let ex_zoom_doc = Call(Zoom, (22, [\"Dr. Zoidberg\"], \"Doctor appointment\"));;\nlet ex_teams_office = Call(Teams, (7, [\"Mike\"; \"Tajel\"], \"Office hours\"));;\nlet ex_phone_spam = Call(Phone, (1, [\"Unknown\"], \"Spam\"));;\n\nlet ex_mtg_study = Mtg(62, [\"Rachel\"; \n                            \"Ross\"; \n                            \"Joey\"; \n                            \"Phoebe\"; \n                            \"Chandler\"; \n                            \"Monica\"], \"Study group\");;\nlet ex_mtg_advisor = Mtg(28, [\"Ali\"], \"Research meeting\");;\n\nlet ex_alone_lunch = Alone(34, \"lunch\");;\nlet ex_alone_reading = Alone(25, \"Reading Infinite Jest\");;\n\nlet assertions () = \n  assert (social_time [] = 0);\n  assert (social_time [ex_zoom_doc; \n                        ex_teams_office; \n                        ex_phone_spam; \n                        ex_mtg_study; \n                        ex_mtg_advisor;\n                        ex_alone_lunch;\n                        ex_alone_reading] = 120)\n;;\n\nassertions()",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "flip_tree",
    "language": "ocaml",
    "prompt": "(* A NumTree is one of:\n- Number NumTree NumTree\n- Number\nInterpretation: A set of numbers arranged in a tree shape. *)\ntype numtree =\n  | Leaf of int\n  | Node of int * numtree * numtree\n\n(* mirror: NumTree -> NumTree\nMirrors the tree around the center point *)\nlet rec mirror (tree : numtree) : numtree =",
    "doctests": "original",
    "original": "ocaml-prompts/flip_tree.ml",
    "prompt_terminology": "verbatim",
    "tests": "let assertions() =\n  assert (mirror (Leaf 5) = Leaf 5);\n  assert (mirror (Node (5, Leaf 6, Leaf 7)) = Node (5, Leaf 7, Leaf 6));\n  assert (mirror (Node (5, Node (8, Leaf 9, Leaf 10), Node (4, Leaf 3, Leaf 2))) =\n          Node (5, Node (4, Leaf 2, Leaf 3), Node (8, Leaf 10, Leaf 9)))\n;;\n\nassertions();;\n",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "add_station",
    "language": "ocaml",
    "prompt": "(* A Station type consists of the name of a station and the names of stations \nthat directly connect to it. *)\ntype station = \n  | Station of string * (string list) \n\n(* A Subway is a list of stations that make a subway network. *)\n\n(* has_connection: checks if a station has a connection to some other station name. *)\nlet has_connection (str: string) (stat: station) : bool = \n  let Station(_, connections) = stat \n    in List.exists (fun t -> String.equal t str) connections\n;;\n\n(* add_edge_to_station: Adds a single edge to a station. *)\nlet add_edge_to_station (str: string) (stat: station) : station = \n    if (has_connection str stat) \n    then stat \n    else \n      let Station(name, connections) = stat in Station(name, str::connections)\n;;\n\n(* add_edge : adds a connection to the subway, if it does not\nalready exist. Assumes that both from and to stations in the Subway. *)\nlet rec add_edge (from: string) (towards: string) (sub: station list) : station list = ",
    "doctests": "original",
    "original": "ocaml-prompts/add_station.ml",
    "prompt_terminology": "verbatim",
    "tests": "let ex_station_1 = Station(\"Newton Centre\", [\"Fenway\"; \"Kenmore\"]) ;;\nlet ex_station_2 = Station(\"Fenway\", [\"Newton Highlands\"; \"Newton Centre\"]);; \nlet ex_station_3 = Station(\"Kenmore\", [\"Newton Centre\"]);;\nlet ex_station_4 = Station(\"Newton Highlands\", [\"Fenway\"]);;\n\nlet ex_subway_1 = [ex_station_1; ex_station_2; ex_station_3; ex_station_4];;\nlet ex_subway_2 = [];;\nlet ex_subway_3 = [\n  Station(\"A\", [\"B\"; \"D\"]);\n  Station(\"B\", [\"C\"; \"A\"]);\n  Station(\"C\", [\"D\"; \"B\"]);\n  Station(\"D\", [\"A\"; \"C\"]);\n];;\n\nlet assertions () = \n  assert (add_edge \"Fenway\" \"Kenmore\" ex_subway_2 = ex_subway_2);\n  assert (add_edge \"Fenway\" \"Kenmore\" ex_subway_1 = [ \n    ex_station_1;\n    Station(\"Fenway\", [\"Kenmore\"; \"Newton Highlands\"; \"Newton Centre\"]);\n    ex_station_3;\n    ex_station_4;\n  ]);\n  assert (add_edge \"Kenmore\" \"Newton Centre\" ex_subway_1 = ex_subway_1);\n  assert (add_edge \"A\" \"C\" ex_subway_3 = [ \n    Station(\"A\", [\"C\"; \"B\"; \"D\"]);\n    Station(\"B\", [\"C\"; \"A\"]);\n    Station(\"C\", [\"D\"; \"B\"]);\n    Station(\"D\", [\"A\"; \"C\"])\n  ])\n;;\n\nassertions();;",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "apply_to_android",
    "language": "ocaml",
    "prompt": "(* A Phone is one of:\n- iphone [num_version]\n- android [str_version]\n- pixel [num_version]\nInterpretation: A type of smartphone *)\ntype phone = \n    | Iphone of int\n    | Android of string\n    | Pixel of int\n;;\n\n(* apply_to_android:\nApplies a func to the androids in the list of phones *)\nlet rec apply_to_android (f: 'a -> 'b) (p: phone list) : phone list = ",
    "doctests": "original",
    "original": "ocaml-prompts/apply_to_android.ml",
    "prompt_terminology": "verbatim",
    "tests": "let uppercase_android (p : phone) : phone = \n    match p with\n    | Android(s) -> Android(String.uppercase_ascii(s))\n    | _ -> p\n  ;;\n\nlet reverse_android (p : phone) : phone = \n    match p with\n    | Android(s) -> Android(String.concat \"\" ( List.rev (Str.split (Str.regexp \"\") \"Cupcake\")))\n    | _ -> p\n  ;;\n\nlet version_android (p : phone) : phone = \n    match p with\n    | Android(s) -> Android(s ^ \"-v2\")\n    | _ -> p\n  ;;\n\nlet assertions () = \n    assert (apply_to_android uppercase_android [] = []);\n    assert (apply_to_android uppercase_android \n        [Iphone(5); Pixel(3); Android(\"Cupcake\"); Pixel(7); Android(\"Grape\"); Iphone(10); Android(\"Orange\")] = \n        [Iphone(5); Pixel(3); Android(\"CUPCAKE\"); Pixel(7); Android(\"GRAPE\"); Iphone(10); Android(\"ORANGE\")]);\n    assert (apply_to_android reverse_android\n        [Iphone(5); Pixel(3); Android(\"Cupcake\")] = \n        [Iphone(5); Pixel(3); Android(\"ekacpuC\")]);\n    assert (apply_to_android version_android\n        [Iphone(5); Pixel(3)] = \n        [Iphone(5); Pixel(3)])\n  ;;\n\nassertions()\n",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "collatz",
    "language": "ocaml",
    "prompt": "(* Collatz: Number ->  Number\nCounts how many steps it takes a number to converge\nto 1 through the collatz sequence. The collatz sequence\ndivides by 2 if the number is even, otherwise if the number\nis odd, it multiplies by 3 and adds 1 *)\nlet rec collatz (num : int) : int =",
    "doctests": "original",
    "original": "ocaml-prompts/collatz.ml",
    "prompt_terminology": "verbatim",
    "tests": "let assertions() =\n  assert (collatz 1 = 0);\n  assert (collatz 2 = 1);\n  assert (collatz 4 = 2);\n  assert (collatz 3 = 7);\n  assert (collatz 12 = 9)\n;;\n\nassertions();;",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  },
  {
    "name": "shapes",
    "language": "ocaml",
    "prompt": "(* A Shape is one of:\n * - Circle of float\n * - Rectangle of float * float\n * - Triangle of float * float * float *)\n type shape =\n | Circle of float\n | Rectangle of float * float\n | Triangle of float * float * float\n\n(* circle_perimeter: shape -> float *)\n(* Computes the perimeter of a circle *)\nlet circle_perimeter c =\n 6.28 *. c\n\n(* rectangle_perimeter: shape -> float *)\n(* Computes the perimeter of a rectangle *)\nlet rectangle_perimeter r =\n 2.0 *. (fst r) +. 2.0 *. (snd r)\n\n(* triangle_perimeter: shape -> float *)\n(* Computes the perimeter of a triangle *)\nlet triangle_perimeter t =\n let (s1, s2, s3) = t in\n s1 +. s2 +. s3\n\n(* shape_perimeter: shape -> float *)\n(* Computes the perimeter of any Shape *)\nlet shape_perimeter (s : shape) : float =",
    "doctests": "original",
    "original": "ocaml-prompts/shapes.ml",
    "prompt_terminology": "verbatim",
    "tests": "let assertions() =\n assert (shape_perimeter (Circle 2.0) = 12.56);\n assert (shape_perimeter (Rectangle (2.0, 4.0)) = 12.0);\n assert (shape_perimeter (Triangle (3.0, 3.0, 3.0)) = 9.0)\n;;\n\nassertions();;",
    "stop_tokens": [
      "\n\n",
      "\n(*",
      "\ntype"
    ]
  }
]