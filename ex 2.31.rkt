#lang sicp
(define (map proc items)
    (if (null? items) '() 
        (cons (proc (car items)) (map proc (cdr items)))))

(define (tree-map f x)
    (map 
        (lambda (x) 
            (if (pair? x) (tree-map f x) 
                (f x)))
    x))

(define (square-tree tree) (tree-map square tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))