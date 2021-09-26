(define (is-loop? x)
  (define p '())
  (define (is-loop l)
    (define (loop-current o l)
      (cond ((not (pair? l)) #f)
            ((null? l) #f)
            ((eq? o (cdr l)) #t)
            ((pair? (car l)) (set! p (cons l p)) (is-loop (car l)))
            (else (loop-current o (cdr l)))))
    (if (eq? p l) #t (loop-current l l)))
  (is-loop x))





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

(define t1 (list '1 x '2))
(define t2 (list '1 x1 '2))


(is-loop? x)
;; #t
(is-loop? x1)
;; #t
(is-loop? (cons x2 y2))
;; #f
