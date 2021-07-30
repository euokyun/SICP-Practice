#lang sicp

; (define (union-set set1 set2)
;     (cond 
;         ((and (null? set1) (null? set2)) '())
;         ((null? set1) set2)
;         ((null? set2) set1)
;         (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))
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

(define (union-set set1 set2) ; 두 set을 합침
    (cond
        ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (union-set (cdr set1) set2))
        ((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))))

(union-set '(2 4 6 8) '(1 3 5 7)) ;(1 2 3 4 5 6 7 8)
(union-set '() '(1 3 5 7)) ;(1 3 5 7)
(union-set '(2 4 6 8) '()) ;(2 4 6 8)
(union-set '(1 2 3) '(2 3 4)) ;(1 2 3 4)
