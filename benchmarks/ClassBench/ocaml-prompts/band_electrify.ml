(* A Guitar is a [string string bool]
Interpretation: A [brand_name color electric] represents a guitar where:
- brand_name is the brand name of the guitar
- color is the color of the guitar
- electric? is a boolean indicating whether the guitar is electric *)
type guitar =
  | Guitar of string * string * bool

(* A DrumKit is a [string bool]
Interpretation: A [brand_name electric?] represents
a drum kit where:
- brand_name is the brand name of the drum kit
- electric? is a boolean indicating whether the drum kit is electric *)
type drum_kit =
  | DrumKit of string * bool

(* A Saxophone is a [string]
Interpretation: A [brand_name] represents a saxophone where:
- brand_name is the brand name of the saxophone *)
type saxophone =
  | Saxophone of string

(* A Piano is a [string]
Interpretation: A [brand_name] represents a piano where:
- brand_name is the brand name of the piano *)
type piano =
  | Piano of string

(* An Instrument is one of:
- Guitar
- DrumKit
- Saxophone
- Piano
Interpretation: An Instrument is one of the four musical instruments. *)
type instrument =
  | Guitar of guitar
  | DrumKit of drum_kit
  | Saxophone of saxophone
  | Piano of piano

(* A Band is one of:
A Band is one of:
- OnePieceBand instrument
- TwoPieceBand instrument instrument
- ThreePieceBand instrument instrument instrument
Interpretation: A Band represents a band with one, two, or three members. *)
type band =
  | OnePieceBand of instrument
  | TwoPieceBand of instrument * instrument
  | ThreePieceBand of instrument * instrument * instrument

(* electrify-guitar : Makes this an electric guitar! *)
let electrify_guitar (g : guitar) : guitar =
  match g with
  | Guitar (brand_name, color, _) -> Guitar (brand_name, color, true)
;;

(* electrify-drum-kit : Makes this an electric drum kit! *)
let electrify_drum_kit (d : drum_kit) : drum_kit =
  match d with
  | DrumKit (brand_name, _) -> DrumKit (brand_name, true)
;;

(* electrify-instrument : Makes this an electric instrument! *)
let electrify_instrument (i : instrument) : instrument =
  match i with
  | Guitar g -> Guitar (electrify_guitar g)
  | DrumKit d -> DrumKit (electrify_drum_kit d)
  | _ -> i
;;

(* electrify-band : Makes this an electric band! *)
let electrify_band (b : band) : band =
  (* <solution> *)
  match b with
  | OnePieceBand i -> OnePieceBand (electrify_instrument i)
  | TwoPieceBand (i1, i2) -> TwoPieceBand (electrify_instrument i1, electrify_instrument i2)
  | ThreePieceBand (i1, i2, i3) -> ThreePieceBand (electrify_instrument i1, electrify_instrument i2, electrify_instrument i3)
;;

(* <tests> *)
let assertions () =
  assert ((electrify_band (OnePieceBand (Guitar (Guitar ("Stratocaster", "red", true))))) = 
        OnePieceBand (Guitar (Guitar ("Stratocaster", "red", true))));
  assert ((electrify_band (TwoPieceBand (DrumKit (DrumKit ("Korg", true)), DrumKit (DrumKit ("Korg", false))))) = 
        TwoPieceBand (DrumKit (DrumKit ("Korg", true)), DrumKit (DrumKit ("Korg", true))));
  assert ((electrify_band (ThreePieceBand (Guitar (Guitar ("Stratocaster", "red", true)), DrumKit (DrumKit ("Korg", false)), Saxophone (Saxophone ("Yamaha"))))) = 
        ThreePieceBand (Guitar (Guitar ("Stratocaster", "red", true)), DrumKit (DrumKit ("Korg", true)), Saxophone (Saxophone ("Yamaha"))));
;;

assertions()
