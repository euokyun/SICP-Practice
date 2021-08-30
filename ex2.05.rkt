#lang sicp
(define (square n) (* n n))

(define (fast-expt b n)
    (cond  
        ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (cons x y)
    (* (expt 2 x) (expt 3 y)))

(display (cons 1 2))