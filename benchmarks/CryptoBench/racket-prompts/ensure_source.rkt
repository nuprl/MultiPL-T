#lang racket
(require rackunit)
(require crypto)
(require crypto/libcrypto)
(crypto-factories libcrypto-factory)

(define rsa-impl (get-pk 'rsa libcrypto-factory))
(define signature-mode 'sha256)

;; ensure_source: Bytes Bytes Bytes -> Boolean
;; Verifies the identity of an author who sent the message.
;; Returns True if the author did write the message
(define (ensure_source message signature author_public_key)
;; <solution>
    (digest/verify author_public_key signature-mode message signature))

;; <tests>
(define privkey-a (generate-private-key rsa-impl '((nbits 2048))))
(define privkey-b (generate-private-key rsa-impl '((nbits 2048))))
(define pubkey-a (pk-key->public-only-key privkey-a))
(define pubkey-b (pk-key->public-only-key privkey-b))
(define message #"A great message")
(define sig-a (digest/sign privkey-a signature-mode message))
(define sig-b (digest/sign privkey-b signature-mode message))

(check-equal? (ensure_source message sig-a pubkey-a) #t)
(check-equal? (ensure_source message sig-b pubkey-a) #f)
(check-equal? (ensure_source message sig-a pubkey-b) #f)
(check-equal? (ensure_source message sig-b pubkey-b) #t)