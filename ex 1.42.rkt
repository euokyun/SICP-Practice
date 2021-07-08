#lang sicp
(define (compose f g)
    (lambda (x) (f (g x))))
(define (inc a) (+ a 1))


((compose square inc) 6)