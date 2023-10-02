#lang racket
(require rackunit)
(require crypto)
(require crypto/libcrypto)
(crypto-factories libcrypto-factory)

(define aes-mode '(aes gcm))
(define keygen-digest 'sha256)

;; start_aes: Bytes Bytes -> Bytes
;; Receives a public key and the password and uses the
;; password to generate an AES key via SHA256 and encrypts
;; the key to send it back to the user
(define (start_aes pubkey password)
;; <solution>
    (pk-encrypt pubkey (digest keygen-digest password)))

;; <tests>
(define rsa-impl (get-pk 'rsa libcrypto-factory))
(define (test password)
    (local
        [(define privkey (generate-private-key rsa-impl '((nbits 2048))))
        (define pubkey (pk-key->public-only-key privkey))]
        (check-equal? (pk-decrypt privkey (start_aes pubkey password)) (digest keygen-digest password))))

(test #"Hello")
(test #"goodbye")
(test #"a wonderful evening")