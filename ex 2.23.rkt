#lang sicp

; (define (foreach f l)
;     (if (null? l) #t (f (car l)))
;     (if (null? l) #t 
;         (foreach f (cdr l))))

(define (foreach f l)
    (if (null? l) #t 
        ((lambda () (f (car l)) (foreach f (cdr l))))))

(foreach 
    (lambda (x) (newline) (display x)) (list 57 321 88))