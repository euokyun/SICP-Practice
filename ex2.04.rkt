#lang sicp
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p g) p)))
(define (cdr z) (z (lambda (p g) g)))

(define test-1 (cons 3 8))
(define test-2 (car test-1))
(define test-3 (cdr test-1))
(display test-1)
(display test-2)
(display test-3)