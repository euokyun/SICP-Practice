#lang sicp
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define codesample '(B A C A D A E A F A B B A A A G A H))
(define fixed-length-code 
    (lambda (x) (cond
                    ((equal? x 'A) '(0 0 0))
                    ((equal? x 'B) '(0 0 1))
                    ((equal? x 'C) '(0 1 0))
                    ((equal? x 'D) '(0 1 1))
                    ((equal? x 'E) '(1 0 0))
                    ((equal? x 'F) '(1 0 1))
                    ((equal? x 'G) '(1 1 0))
                    ((equal? x 'H) '(1 1 1)))))
(define variable-length-code
    (lambda (x) (cond
                    ((equal? x 'A) '(0))
                    ((equal? x 'B) '(1 0 0))
                    ((equal? x 'C) '(1 0 1 0))
                    ((equal? x 'D) '(1 0 1 1))
                    ((equal? x 'E) '(1 1 0 0))
                    ((equal? x 'F) '(1 1 0 1))
                    ((equal? x 'G) '(1 1 1 0))
                    ((equal? x 'H) '(1 1 1 1)))))
(let ((fixed-result (flatmap fixed-length-code codesample))
      (vari-result (flatmap variable-length-code codesample)))
    (for-each (lambda (x) (display x)) fixed-result)
    (newline)
    (display (length fixed-result))
    (newline)
    (for-each (lambda (x) (display x)) vari-result)
    (newline)
    (display (length vari-result))
    (newline)
    (display (- 100 (* 100.0 (/ (length vari-result) (length fixed-result)))))
    (display '%)
)


(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define symbol-leaf cadr)
(define weight-leaf caddr)

(define (make-code-tree left right)
    (list 
        left 
        right 
        (append (symbols left) (symbols right)) 
        (+ (weight left) (weight right))))

(define left-branch car)
(define right-branch cadr)
(define (symbols tree)
    (if (leaf? tree) (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree) (weight-leaf tree)
        (cadddr tree)))

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