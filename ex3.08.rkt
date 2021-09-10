#lang sicp
(define (fn) 
    (let ([init -1])
        (lambda (a) 
            (cond 
                [(eq? init -1) (begin (set! init a) init)]
                [else init]))))
(define f (fn))
(+ (f 0) (f 1)) ; 0