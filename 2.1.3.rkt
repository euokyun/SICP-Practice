#lang sicp
(define (cons x y)
    (define (dispatch m)
        (cond ((=m 0) x)
              ((=m 1) y)
              (else (error "Argument not 0 or 1 -- CONS" m))))
    dispatch) ;<-결과를 프로시저로 내놓음

(define (car z) (z 0))
(define (cdr z) (z 1))