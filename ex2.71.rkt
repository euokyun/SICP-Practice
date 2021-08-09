#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define entry car)
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree) (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree) (weight-leaf tree)
        (cadddr tree)))
(define (make-code-tree left right)
    (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))
(define (element-of-set? x set) 
   (cond ((null? set) false) 
         ((equal? x (car set)) #t) 
         (else (element-of-set? x (cdr set))))) 

(define (encode message tree)
    (if (null? message) '()
        (append (encode-symbol (car message) tree) (encode (cdr message) tree))))
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
(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits) '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch) (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))
(define (choose-branch bit branch)
    (cond 
        ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOSSE-BRANCH" bit))))
(define (adjoin-set x set)
    (cond
        ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
    (if (null? pairs) '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs))))))
(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))
(define (successive-merge leaf-sets)
    (define (successive-merge-iter leafs)
        (cond 
            ((null? (cdr leafs)) (car leafs))
            (else 
                (successive-merge-iter 
                    (adjoin-set (make-code-tree (car leafs) (cadr leafs)) (cddr leafs))))))
    (successive-merge-iter leaf-sets))

; n=5
(define n5 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))
; n=10
(define n10 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512))))

(length (encode '(A) n5)) ; 4 bit
(length (encode '(E) n5)) ; 1 bit
(length (encode '(A) n10)) ; 9 bit
(length (encode '(J) n10)) ; 6 bit