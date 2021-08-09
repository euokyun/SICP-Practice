#lang sicp
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree) (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree) (weight-leaf tree)
        (cadddr tree)))
(define (make-code-tree left right)
    (list 
        left 
        right 
        (append (symbols left) (symbols right)) 
        (+ (weight left) (weight right))))
(define (element-of-set? x set) 
   (cond ((null? set) false) 
         ((equal? x (car set)) #t) 
         (else (element-of-set? x (cdr set))))) 

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                                    (make-code-tree (make-leaf 'D 1)
                                                    (make-leaf 'C 1)))))

(define (encode message tree)
    (if (null? message) '()
        (append 
            (encode-symbol (car message) tree)
            (encode (cdr message) tree))))
; wrong
; (define (encode-symbol word tree) 
;     (display tree) (newline)
;     (cond
;         ((not (pair? (right-branch tree))) 
;             (if (equal? word (car (symbols tree)) )
;                 (cons 1 '())
;                 ((error ("bad word -- ENCODE-SYMBOL" word)))))
;         ((equal? word (car (symbols (left-branch tree)))) (cons 0 '()))
;         (else (cons 1 (encode-symbol word (right-branch tree))))))

(define (encode-symbol sym tree)
    (define (encode-symbol-1 s t)
        (cond
            ((leaf? t) '())
            ((element-of-set? s (symbols (left-branch t)))
                (cons 0 (encode-symbol s (left-branch t))))
            ((element-of-set? s (symbols (right-branch t))) 
                (cons 1 (encode-symbol s (right-branch t))))))
    (if (element-of-set? sym (symbols tree))
        (encode-symbol-1 sym tree)
        (error "wrong symbol -- ENCODE-SYMBOL" sym)))

(encode '(A D A B B C A) sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 1 0)