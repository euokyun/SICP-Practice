#lang sicp
(define (last-pair lst)
(display (car lst))(newline)
    (if [null? (cdr lst)] lst
        (last-pair (cdr lst))))

(define (make-cycle x)
    (set-cdr! (last-pair x) x) 
    x)

(define z (make-cycle (list 'a 'b 'c)))
z
; (last-pair z)