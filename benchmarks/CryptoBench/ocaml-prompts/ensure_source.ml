#use "topfind";;
#require "cryptokit";;
open Cryptokit
open Printf

(* ensure_source: RSA.key String String -> Boolean
Verifies the identity of an author who sent the message.
Returns True if the author did write the message.
Public key is an RSA public key and message can be 
signed to verify it matches given signature*)
let ensure_source (key: RSA.key) (signature: string) (message: string) : bool =
(* <solution> *)
  (RSA.sign key message) = signature
;;

(* <tests> *)
let keypair_a = RSA.new_key 2048;;
let keypair_b = RSA.new_key 2048;;
let message = Random.string Random.secure_rng 255;;
let message_a = RSA.sign keypair_a message;;
let message_b = RSA.sign keypair_b message;;

assert(ensure_source keypair_a message_a message = true);;
assert(ensure_source keypair_a message_b message = false);;
assert(ensure_source keypair_b message_a message = false);;
assert(ensure_source keypair_b message_b message = true);;