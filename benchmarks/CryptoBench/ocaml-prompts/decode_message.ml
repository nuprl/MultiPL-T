#use "topfind";;
#require "cryptokit";;
open Cryptokit

let padding = Cryptokit.Padding.length;;

(*decode_message string string -> string
Decrypts an encrypted message provided the aes key.
The given message is encoded in aes and padded using
the padding constant.*)
let decode_message (key: string) (encoded_message: string) : string =
(* <solution> *)
  let aes = Cryptokit.Cipher.(aes ~pad:padding key Decrypt) in
  let decoded_message = Cryptokit.transform_string aes encoded_message in
  decoded_message
;;

(* <tests> *)
let test_message (message: string) = 
  let key = Random.string Random.secure_rng 32 in 
  let aes = Cryptokit.Cipher.(aes ~pad:Cryptokit.Padding.length key Encrypt) in
  let encoded_message = Cryptokit.transform_string aes message in
  assert(decode_message key encoded_message = message);;

test_message("A good message");;
test_message("A great day to live");;
test_message("No time to die");;