#lang sicp
; from 2.2.1
(define (append x y)
    (if [null? x] y
        (cons (car x) (append (cdr x) y))))

(define (append! x y)
    (set-cdr! (last-pair x) y) 
    x)

(define (last-pair lst)
    (if [null? (cdr lst)] lst
        (last-pair (cdr lst))))

(define x '(a b))
(define y '(c d))
(define z (append x y))

z ; (a b c d)

(cdr x) ; (b)

(define w (append! x y))

w ; (a b c d)

(cdr x) ; (b c d)