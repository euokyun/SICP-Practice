#lang sicp

(define xxx '((a b) c d))
(define yyy '(e f))
; (set-car! xxx yyy)
; xxx ; ((e f) c d)
; (set-cdr! xxx yyy)
; xxx ; ((a b) e f)
; (define (get-new-pair) '(() ()))
; (define (cons x y)
;     (let ([new (get-new-pair)])
;         (set-car! new x)
;         (set-cdr! new y)
;         new))

; ---

(define x (list 'a 'b))
(define z1 (cons x x))
z1 ; ((a b) a b)
(define z2 (cons (list 'a 'b) (list 'a 'b)))
z2 ; ((a b) a b)

(define (set-to-wow! x)
    (set-car! (car x) 'wow) 
    x)

(set-to-wow! z1) ; ((wow b) wow b)
(set-to-wow! z2) ; ((wow b) a b)
; (eq? (car z1) (cdr z1)) ; #t
; (eq? (car z2) (cdr z2)) ; #f