#lang sicp
(define (map proc items)
    (if (null? items) '() 
        (cons (proc (car items)) (map proc (cdr items)))))

(define (square-list items)
    (if (null? items) '()
        (cons (* (car items) (car items)) 
              (square-list (cdr items)))))
(square-list (list 1 2 3 4))
;Value: (1 4 9 16)
(define (square-list items)
    (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4))
;Value: (1 4 9 16)