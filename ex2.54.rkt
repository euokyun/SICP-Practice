#lang sicp
(equal? '(this is a list) '(this is a list))
; #t
(equal? '(this is a list) '(this (is a) list))
; #f

(define (new-equal? a b)
    (if (and (pair? a) (pair? b))
        (and (eq? (car a) (car b)) (new-equal? (cdr a) (cdr b)))
        ; (if (eq? (car a) (car b))
        ;     (new-equal? (cdr a) (cdr b))
            ; #f)
        (eq? a b)))

(new-equal? '(this is a list) '(this is a list))
; #t
(new-equal? '(this is a list) '(this (is a) list))
; #f