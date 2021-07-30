#lang sicp
(define (list-ref items n)
    (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))

; (define (length items)
;     (if (null? items) 0 (+ 1 (length (cdr items)))))

(define (length items)
    (define (length-iter a count)
        (if (null? a) count
            (length-iter (cdr a) (+ 1 count))))
    (length-iter items 0))

(define (append list1 list2)
    (if (null? list1) list2
        (cons (car list1) (append (cdr list1) list2))))

(define (last-pair items)
    (if (null? (cdr items)) (car items) (last-pair (cdr items))))

; (define (reverse items)
;     (if 
;         (null? (cdr items)) 
;         (list (car items)) 
;         (append (reverse (cdr items)) (list (car items)))))

(define (reverse items)
    (define (rev-iter i r)
        (if (null? i) r 
            (rev-iter (cdr i) (append (list (car i)) r))))
    (rev-iter items (list)))

(reverse (list 1 2 2 5 3 7 3))