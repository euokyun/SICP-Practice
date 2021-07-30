#lang sicp
(define (append list1 list2)
    ;; (newline) (display list1) 
    ;; (newline) (display list2)
    ;; (newline)
    (if (not (pair? list1)) list2
        (cons (car list1) (append (cdr list1) list2))))


(define x (list (list 1 2) (list 3 4)))

(define (fringe items)
    (if (pair? items)
        (append (fringe (car items)) (fringe (cdr items)))
        (if (not (null? items)) (list items) '())
    ))
(fringe (list 3))
(fringe x)
(fringe (list x x))