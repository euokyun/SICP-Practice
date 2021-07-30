#lang sicp
(define (square n) (* n n))

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
        
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond
        ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
    (= (remainder b a) 0))
(define (prime? n)
    (= n (smallest-divisor n)))
(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
    (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(define (unique-pairs n)
    (if (null? n) (list '())
        (flatmap 
            (lambda (i) 
                (map 
                    (lambda (j) (list i j))
                    (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n))))

(unique-pairs 6)
(prime-sum-pairs 6)