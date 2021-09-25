;; note for mit-scheme
;; mit-scheme do not display cycle pair properly when you use display procedure. it will print infinity ((((((.
(define (is-loop? l)
    (let ((o '()))
        (define (is-loop ps)
            (define (is-loop-1 p)
                ; (display "ps: ") (display ps) (newline)
                ; (display "o: ") (display o) (newline)
                ; (display "p: ") (display p) (newline)
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


(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define z (cons 'e 'f))
(set-car! x z)
(set-car! y x)
(set-car! z y)

(define x1 (cons 'a 'b))
(define y1 (cons 'c 'd))
(define z1 (cons 'e 'f))
(set-cdr! x1 z1)
(set-cdr! y1 x1)
(set-cdr! z1 y1)

(define x2 (cons 'a 'b))
(define y2 (cons 'c 'd))
(define z2 (cons 'e 'f))
(set-cdr! x2 z2)
(set-cdr! y2 z2)
;; (cons x2 y2)

;; (display x)(newline)
;; (display x1)(newline)
;; (display (cons x2 y2))(newline)


(is-loop? x)
(is-loop? x1)
(is-loop? (cons x2 y2))
