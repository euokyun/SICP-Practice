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

;;리스트 매핑

(define (scale-list items factor)
    (if (null? items)
        '()
        (cons (* (car items) factor)
              (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)
;Value: (10 20 30 40 50)
(define (map proc items)
    (if (null? items) '() 
        (cons (proc (car items)) (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

(map (lambda (x) (* x x)) (list 1 2 3 4)))
;Value: (1 4 9 16)
(define (scale-list items factor)
    (map (lambda (x) (* x factor)) items))

(scale-list (list 1 2 3 4 5) 10)
;Value: (10 20 30 40 50)