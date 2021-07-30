#lang sicp

(define (enumerate-interval low high)
    (if (> low high) '()
        (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (filter predicate sequence)
    (cond 
        ((null? sequence) '())
        ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (triple n)
    (if (null? n) (list '())
        (flatmap 
            (lambda (i)
                (map
                    (lambda (j) (list i j (- i j)))
                    (filter 
                        (lambda (a) (> a (/ i 2))) 
                        (enumerate-interval 1 (- i 1)))))
            (reverse (enumerate-interval 1 n)))))

(triple 8)
;Value: ((8 5 3) (8 6 2) (8 7 1) (7 4 3) (7 5 2) (7 6 1) (6 4 2) (6 5 1) (5 3 2) (5 4 1) (4 3 1) (3 2 1))