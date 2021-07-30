#lang sicp
(define (add-interval x y)
    (make-interval  (+ (lower-bound x) (lower-bound y)
                    (+ (upper-bound x) (upper-bound y)))))

(define (mul-interval x y)
    (let (
           (p1 (* (lower-bound x) (lower-bound y)))
           (p2 (* (lower-bound x) (upper-bound y)))
           (p3 (* (upper-bound x) (lower-bound y)))
           (p4 (* (upper-bound x) (upper-bound y)))
        )
        (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))

(let (
        (i1 (make-interval 3 5))
        (i2 (make-interval -2 5))
        (i3 (make-interval 3 -3))
    )
    (display (lower-bound i1))
    (newline)
    (display (lower-bound i2))
    (newline)
    (display (lower-bound i3))
    (newline)
    (display (upper-bound i1))
    (newline)
    (display (upper-bound i2))
    (newline)
    (display (upper-bound i3))
    (newline)
    )