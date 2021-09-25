#lang sicp
; (define (iscycle? x)
;     (define visited '())
;     (define (iscycle?-1 x)
;         )
; )

(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define z (cons 'e 'f))

    (set-car! x z)
    (set-car! y x)
    (set-car! z y)
    ; (iscycle? b) ; endless loop
; (let* ((x (cons 'a 'b))(y (cons 'c 'd))(z (cons 'e 'f))(b x))
;     (set-cdr! x y)
;     (set-cdr! y z)
;     (set-cdr! z x)
;     b
;     ; (iscycle? b) ; endless loop
;     (list 'a b 'c)
; )

(define (is-loop? l)
    (let ((o '()))
        (define (is-loop ps)
            (define (is-loop-1 p)
                (display "ps: ") (display ps) (newline)
                (display "o: ") (display o) (newline)
                (display "p: ") (display p) (newline)
                (cond 
                    ((not (pair? p)) #f)
                    ((null? p) #f)
                    ((memq p o) #t)
                    (else (begin 
                        (set! o (cons p o))
                        (if (pair? (car p)) (is-loop (car p))
                            (is-loop-1 (cdr p)))))))
            (if (memq ps o) #t
                (is-loop-1 ps)))
    (is-loop l)))


; (is-loop? '((1 2 3)))

(define x1 (cons 'a 'b))
(define y1 (cons 'c 'd))
(define z1 (cons 'e 'f))
(set-cdr! x1 z1)
(set-cdr! y1 x1)
(set-cdr! z1 y1)
x1

(is-loop? x)
(is-loop? x1)
(is-loop? '((1 2 3) 4 5 6))


(define x2 (cons 'a 'b))
(define y2 (cons 'c 'd))
(define z2 (cons 'e 'f))
(set-cdr! x2 z2)
(set-cdr! y2 z2)
(is-loop? (cons x2 y2))