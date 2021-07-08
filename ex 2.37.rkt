#lang sicp
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
    (if (null? (car seqs)) '()
        (cons 
            (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))


(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define n (list (list 1 2 3) (list 4 5 6) (list 6 7 8) (list 1 4 6)))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))
;;행렬의 내적

(dot-product (list 2 3) (list 4 5))

(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product v x)) m)) 

(define (transpose mat)
    (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n))) (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-vector m (list 1 2 3 4))
(transpose m) 
(matrix-*-matrix m n)

