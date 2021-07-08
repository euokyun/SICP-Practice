#lang sicp
(define (f g) (g 2))
(define (square x)
    (* x x))
(f square)
;square 2

(f (lambda (z) (* z (+ z 1))))
;lam~ 2

(f f)
;f f = f 2= 2 2 에러(인자 먼저 정의)

