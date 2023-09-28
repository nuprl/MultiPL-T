#lang racket
(require rackunit)
(require crypto)
(require crypto/libcrypto)
(crypto-factories libcrypto-factory)

(define signature-mode 'sha256)

;; ensure_source: Bytes Bytes Bytes -> Boolean
;; Verifies the identity of an author who sent the message.
;; Returns True if the author did write the message.
;; Message given is a digest (aka still encrypted)
(define (ensure_source dgst signature author_public_key)
;; <solution>
    (pk-verify-digest author_public_key signature-mode dgst signature))

;; <tests>
(define rsa-impl (get-pk 'rsa libcrypto-factory))
(define privkey-a (generate-private-key rsa-impl '((nbits 2048))))
(define privkey-b (generate-private-key rsa-impl '((nbits 2048))))
(define pubkey-a (pk-key->public-only-key privkey-a))
(define pubkey-b (pk-key->public-only-key privkey-b))
(define dgst (digest signature-mode #"A great message"))
(define sig-a (pk-sign-digest privkey-a signature-mode dgst))
(define sig-b (pk-sign-digest privkey-b signature-mode dgst))

(check-equal? (ensure_source dgst sig-a pubkey-a) #t)
(check-equal? (ensure_source dgst sig-b pubkey-a) #f)
(check-equal? (ensure_source dgst sig-a pubkey-b) #f)
(check-equal? (ensure_source dgst sig-b pubkey-b) #t)