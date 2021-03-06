#lang sicp
;; (define (reverse items)
;;     (if 
;;         (null? (cdr items)) 
;;         (list (car items)) 
;;         (append (reverse (cdr items)) (list (car items)))
;;     ))

;; (define (reverse items)
;;     (define (rev-iter i r)
;;         (if (null? i) r 
;;             (rev-iter (cdr i) (append (list (car i)) r))
;;         ))
;;     (rev-iter items (list))
;; )

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest) result
            (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence))

(define (fold-right op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (fold-right op initial (cdr sequence)))))
        
; (define (reverse sequence)
;     (fold-right (lambda (x y) (append y (list x))) '() sequence))

(reverse (list 1 2 3 4))


(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) '() sequence))

(reverse (list 1 2 3 4))