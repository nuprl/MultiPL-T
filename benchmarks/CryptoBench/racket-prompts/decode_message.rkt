#lang racket
(require rackunit)
(require crypto)
(require crypto/libcrypto)
(crypto-factories libcrypto-factory)

(define MODE '(aes gcm))

;; decode_message: Bytes Bytes Bytes -> Bytes
;; Decodes the given message with the given key and iv
(define (decode_message key iv encrypted_message)
;; <solution>
    (decrypt MODE key iv encrypted_message))

;; <tests>
;; test_message Bytes -> _
;; Tests if a given message is properly decoded
(define (test_message message)
    (local 
        [(define key (generate-cipher-key MODE))
        (define iv (generate-cipher-iv MODE))
        (define encrypted_message (encrypt MODE key iv message))]
        (check-equal? message (decode_message key iv encrypted_message))))

(test_message #"A good message")
(test_message #"A great day to live")
(test_message #"No time to die")