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
(define (encode message tree)
    (if (null? message) '()
        (append 
            (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

; ((null? tree) (error ("bad word -- ENCODE-SYMBOL" word)))
(define (encode-symbol word tree) 
    (cond
        ((= 1 (weight tree)) 
            (if (equal? word (car (symbols tree)) )
                (cons 1 '())
                ((error ("bad word -- ENCODE-SYMBOL" word)))))
        ((equal? word (car (symbols (left-branch tree)))) (cons 0 '()))
        (else (cons 1 (encode-symbol word (right-branch tree))))))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits) '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch) 
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
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
            (adjoin-set (make-leaf (car pair)       ; 글자 
                                   (cadr pair))     ; 빈도
                        (make-leaf-set (cdr pairs))))))

(define test-pairs '((A 4) (B 2) (C 1) (D 1)))


(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                                    (make-code-tree (make-leaf 'D 1)
                                                    (make-leaf 'C 1)))))



; 글자-빈도 쌍으로 이루어진 리스트를 인자로 받아서 인코딩 트리를 만들어내는 프로시저
(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-sets)
    (define (successive-merge-iter leaf-sets code-tree) 
        (cond 
            ((null? leaf-sets) code-tree)
            ((null? code-tree) (successive-merge-iter (cddr leaf-sets) (make-code-tree (car leaf-sets) (cadr leaf-sets))))
            (else (successive-merge-iter (cdr leaf-sets) (make-code-tree (car leaf-sets) code-tree)))))
    (successive-merge-iter leaf-sets '()))
    
(equal? sample-tree (generate-huffman-tree test-pairs)) #t
(generate-huffman-tree test-pairs) ; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)