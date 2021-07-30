#lang sicp

(define (element-of-set? x set)
    (cond 
        ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define adjoin-set cons)

(define (intersection-set set1 set2)
    (cond 
        ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
            (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
    (cond 
        ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

(adjoin-set 1 '(2 3 4)) ; O(1)
(union-set '(1 2 3 4) '(4 5 6 7)) ; O(n^2)->O(n)
(intersection-set '(1 2 3 4) '(4 5 6 7)) ; O(n^2)