(define (fib-gen a b)
    (let loop ((a a) (b b))
        (cons a (lambda () (loop b (+ a b))))))
