#lang sicp
(define (make-segment start-segment end-segment) 
    (cons start-segment end-segment))
(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment segment)
    (let (
        (mid-x (average (x-point (car segment)) (x-point (cdr segment))))
        (mid-y (average (y-point (car segment)) (y-point (cdr segment)))))
    (make-point mid-x mid-y)))

(define (print-point p)
    (newline)
    (display "x=")
    (display (x-point p))
    (display ", y=")
    (display (y-point p))
)

(define (average a b) (/ (+ a b) 2))

(define test-a (make-point 2 2))
(define test-b (make-point 4 4))
(define test-c (make-segment test-a test-b))
(define test-d (midpoint-segment test-c))
(print-point test-a)
(print-point test-c)
(print-point test-d)
