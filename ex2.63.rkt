#lang sicp
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

(define t1 (make-tree 7 
                (make-tree 3 
                    (make-tree 1 nil nil) 
                    (make-tree 5 nil nil)) 
                (make-tree 9 
                    nil 
                    (make-tree 11 nil nil))))
(define t2 (make-tree 3 
                (make-tree 1 nil nil)
                (make-tree 7 
                    (make-tree 5 nil nil) 
                    (make-tree 9 
                        nil 
                        (make-tree 11 nil nil)))))
(define t3 (make-tree 5
                (make-tree 3 
                    (make-tree 1 nil nil) 
                    nil) 
                (make-tree 9 
                    (make-tree 7 nil nil) 
                    (make-tree 11 nil nil))))

(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list 
                (left-branch tree)
                (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
    (copy-to-list tree '()))

; a
(equal? (tree->list-2 t1) (tree->list-1 t1)) ; #t  
(equal? (tree->list-2 t2) (tree->list-1 t2)) ; #t 
(equal? (tree->list-2 t3) (tree->list-1 t3)) ; #t 

; b
; (define (tree->list-1 tree)
;     (if (null? tree)
;         '()
;         (append (tree->list-1 (left-branch tree))
                ; (cons (entry tree) (tree->list-1 (right-branch tree))))))
(append (tree->list-1 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))))
(append (append 
            (tree->list-1 (make-tree 1 '() '()))
            (cons 3 (tree->list-1 (make-tree 5 '() '())))))

(append (append 
            (tree->list-1 (make-tree 1 '() '()))
            (cons 3 (tree->list-1 (make-tree 5 '() '())))))

(cons 7 (tree->list-1 (make-tree 9 '() (make-tree 11 '() '()))))