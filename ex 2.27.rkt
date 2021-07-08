#lang sicp
(define (append list1 list2)
    (if (null? list1) list2
        (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
    (if 
        (null? (cdr items)) 
        (list (car items)) 
        (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
    (if (null? items) '()
        (append 
            (deep-reverse (cdr items)) 
            (list (if (pair? (car items)) (deep-reverse (car items)) (car items))))))



(deep-reverse (list 1 2))





(define x (list (list 1 2) (list 3 4)))
x
(reverse x) 
;;((3 4) (1 2))
(deep-reverse x)

(deep-reverse (list 1 (list 2 (list 3))))