#lang sicp
(define (subsets s)
    (if (null? s) (list '())
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (x) (if (pair? s) (append (list (car s)) x))) rest)))))

(subsets (list 1 2 3))