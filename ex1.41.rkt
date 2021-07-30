#lang sicp
(define (double f)
    (lambda (d) (f (f d))))
(define (inc a) (+ a 1))

((double inc) 5)
;7
(((double double) inc) 5)
;9
(((double (double double)) inc) 5)
;21
;2^2^2=16번 inc 실행