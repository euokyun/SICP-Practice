#lang sicp
(define (square x) (* x x))


(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum)
    (if (pair? datum) (car datum)
        (error "bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
    (if (pair? datum) (cdr datum)
        (error "bad tagged datum -- CONTENTS" datum)))
(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

;ben
(define real-part-rectangular car)
(define imag-part-rectangular cdr)
(define (magnitude-rectangular z) (sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z)))))
(define (angle-rectangular z) (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
    (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a) 
    (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

; alyssa
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))
(define magnitude-polar car)
(define angle-polar cdr)
(define (make-from-real-imag-polar x y) 
    (attach-tag 'polar (cons (sqrt (+ (square x) (square y))) (atan y x))))
(define (make-from-mag-ang-polar r a) 
    (attach-tag 'polar (cons r a)))

(define (real-part z)
    (cond 
        ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
    (cond 
        ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
    (cond 
        ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
    (cond 
        ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

(define (add-complex z1 z2) ;nothing changed
    (make-from-real-imag 
        (+ (real-part z1) (real-part z2))
        (+ (imag-part z1) (imag-part z2))))

(define make-from-real-imag make-from-real-imag-rectangular)
(define make-from-mag-ang make-from-mag-ang-polar)