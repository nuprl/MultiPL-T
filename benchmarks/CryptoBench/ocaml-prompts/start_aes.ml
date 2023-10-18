#use "topfind";;
#require "cryptokit";;
open Cryptokit

(* start_aes: RSA.key string  -> string
Starts a symmetric key encryption process by generating an 
AES key from the secret using SHA256 and encrypting it with 
the RSA key.*)
let start_aes (rsa_key: RSA.key) (secret: string) : string =
(* <solution> *)
  let aes_key = hash_string (Hash.sha256()) secret in
  (RSA.encrypt rsa_key (aes_key))
;;

(* <tests> *)
let test (secret: string) =
  let keypair = RSA.new_key 4096 in
  let aes_key = hash_string (Hash.sha256()) secret in
  let encrypted_key = (RSA.encrypt keypair (aes_key)) in
  assert(start_aes keypair secret = encrypted_key)
;;

test("A good secret!");;
test("A terrible evening!");;
test("Disaster!");;