#lang sicp
(define one-through-four (list 1 2 3 4))

(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))

(define (list-ref items n)
    (if (= n 0) (car items) (list-ref (cdr items) (- n 1)))))

(list-ref (list 1 2 3 4 5 6) 0)

(define (length items)
    (if (null? items) 0 (+ 1 (length (cdr items)))))

(length (list 1 2 3 4 5 6))

(define (length items)
    (define (length-iter a count)
        (if (null? a) count
            (length-iter (cdr a) (+ 1 count))))
    (length-iter items 0)
)

(define (append list1 list2)
    (if (null? list1) list2)
        (cons (car list1) (append (cdr list1) list2))))

(define (last-pair items)
    (if (null? (cdr items)) (car items) (last-pair (cdr items))))


(last-pair (list 23 72 149 34))