#lang sicp

(define (make-accumulator n)
    (let ([res n])
        (lambda (n) (begin (set! res (+ res n)) res))))

(define A (make-accumulator 5))
(A 10) ; 15
(A 10) ; 25
(define B (make-accumulator 15))
(B -10) ; 5
(B -10) ; -5