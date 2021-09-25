#lang sicp
(define (count-pairs x)
    (if (not (pair? x)) 0
        (+  (count-pairs (car x)) 
            (count-pairs (cdr x)) 
            1)))

(let* ((x (cons 'a 'b))(y (cons 'c 'd))(z (cons 'e 'f))(b x))
    (pair? (car b))
    (set-car! b y)
    (set-car! y z)
    b
    (count-pairs b) ; 3
)

(let* ((x (cons 'a 'b))(y (cons 'c 'd))(z (cons 'e 'f))(b x))
    (set-car! x y)
    (set-car! y z)
    (set-cdr! y z)
    b
    (count-pairs b) ; 4
)

(let* ((x (cons 'a 'b))(y (cons 'c 'd))(z (cons 'e 'f))(b x))
    (set-car! x y)
    (set-cdr! x y)
    (set-car! y z)
    (set-cdr! y z)
    b
    (count-pairs b) ; 7
)

(let* ((x (cons 'a 'b))(y (cons 'c 'd))(z (cons 'e 'f))(b x))
    (set-cdr! x y)
    (set-cdr! y z)
    (set-cdr! z x)
    b
    ; (count-pairs b) ; endless loop
)

