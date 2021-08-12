#lang sicp
; from 2.3.2
(define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2)) 
        (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

(define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))

(define (intall-polynomial-package)
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

    ; adjoin-term, the-empty-termlist
    (define (adjoin-set x set) ;append new term to term list
        (if (element-of-set? x set)
            set
            (cons x set)))
    (define the-empty-termlist '()) ; generate empty term list

    (define (empty-termlist? term-list) (null? term-list))
    (define (first-term term-list) (car term-list))
    (define (rest-term term-list) (cdr term-list))
    ; (define (make-term order coeff) ; 
    ;     ()) 
    ; (define order) 
    ; (define coeff) 
)