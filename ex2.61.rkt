#lang sicp

(define (unordered_list)
    (define (element-of-set? x set)
        (cond 
            ((null? set) #f)
            ((equal? x (car set)) #t)
            (else (element-of-set? x (cdr set)))))

    (define (adjoin-set x set)
        (if (element-of-set? x set)
            set
            (cons x set)))

    (define (intersection-set set1 set2)
        (cond 
            ((or (null? set1) (null? set2)) '())
            ((element-of-set? (car set1) set2)
                (cons (car set1) (intersection-set (cdr set1) set2)))
            (else (intersection-set (cdr set1) set2))))
    (adjoin-set 1 '(2 3 4)))
(unordered_list)
; ordered_list

(define (element-of-set? x set)
    (cond 
        ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2)) 
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond 
                ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
                ((< x1 x2) (intersection-set (cdr set1) set2))
                ((> x1 x2) (intersection-set set1 (cdr set2)))))))
; O(n)

(define (adjoin-set x set)
    (if (null? set) 
        (list x)
        (cond 
            ((= x (car set)) set)
            ((> x (car set)) 
                (if (null? (cdr set)) (list (car set) x)
                    (cons (car set) (adjoin-set x (cdr set)))))
            ((< x (car set)) (cons x set)))))

(adjoin-set 1 '())
(adjoin-set 1 '(2 3 5))
(adjoin-set 4 '(2 3 5))
(adjoin-set 6 '(2 3 5))
