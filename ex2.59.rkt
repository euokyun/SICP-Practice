#lang sicp

(define (element-of-set? x set)
    (cond 
        ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))
        
(define (union-set set1 set2)
    (cond 
        ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

(union-set '(1 2 3 4) '(4 5 6 7)) ; O(n^2)