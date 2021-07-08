#lang sicp
(#%require sicp-pict)

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect a b)
    (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))
(define (sub-vect a b)
    (cons (- (car a) (car b)) (- (cdr a) (cdr b))))
(define (scale-vect s v)
    (cons (* s (car v)) (* s (cdr v))))

(define vect1 (make-vect 3 3))
(define vect2 (make-vect 1 4))

