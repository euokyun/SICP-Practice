#lang sicp
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (map p sequence) 
    (accumulate (lambda (x y) (append (list (p x)) (map p (cdr sequence)))) 
                '() 
                sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(append (list 1 2) (list 3 (list 4)))
(append (list 1) (list 2))

(map square (list 1 2 3 4 5))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3 4 5 6 1))
