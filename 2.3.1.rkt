#lang sicp

; (a b c d)
; (23 45 17)
; ((Norah 12) (Molly 9) (Anna 7) (Lauren 6) (Charlotte 4))

; (* (+ 23 45) (+ x 9))

(define (fact n)
    (if (= n 1)
        1
        (* n (fact (- n 1)))))

(define a 1)
(define b 2)
(list a b)
; (1 2)
(list 'a 'b)
; (a b)
(list 'a b)
; (a 2)
(car '(a b c))
; a 
(cdr '(a b c))
; (b c)
(eq? nil '())
; #t

(define (memq item x)
    (cond 
        ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
; #f
(memq 'apple '(x (apple sauce) y apple pear))
; (apple pear)