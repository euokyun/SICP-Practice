#lang sicp
; ordered tree
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)
(define (list->tree elements)
    (define (partial-tree elts n)
        (if (= n 0)
            (cons '() elts)
            (let ((left-size (quotient (- n 1) 2)))
                (let ((left-result (partial-tree elts left-size)))
                    (let ((left-tree (car left-result))
                        (non-left-elts (cdr left-result))
                        (right-size (- n (+ left-size 1))))
                        (let ((this-entry (car non-left-elts))
                            (right-result (partial-tree (cdr non-left-elts) right-size)))
                            (let ((right-tree (car right-result))
                                (remaining-elts (cdr right-result)))
                                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))
    (car (partial-tree elements (length elements))))
(define (tree->list tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list 
                (left-branch tree)
                (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
    (copy-to-list tree '()))
(define test-set1 '(1 3 5 7 9 11))
(define test-set2 '(1 2 3 4 5 6 7))

(define test-tree1 '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))))
(define test-tree2 '(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ()))))

(define (element-of-set? elem set)
    (cond
        ((null? set) '())
        ((= elem (entry set)) elem)
        ((> elem (entry set)) (element-of-set? elem (right-branch set)))
        ((< elem (entry set)) (element-of-set? elem (left-branch set)))))
; (element-of-set? 5 test-tree2)


(define (intersection-set set1 set2) ; 두 set의 교집합
    (define (inter-iter set1 set2)
        (if (or (null? set1) (null? set2)) 
            '()
            (let ((x1 (car set1)) (x2 (car set2)))
                (cond 
                    ((= x1 x2) (cons x1 (inter-iter (cdr set1) (cdr set2))))
                    ((< x1 x2) (inter-iter (cdr set1) set2))
                    ((> x1 x2) (inter-iter set1 (cdr set2)))))))
    (list->tree (inter-iter (tree->list set1) (tree->list set2))))



(define (union-set set1 set2) ; 두 set을 합침
    (define (union-iter set1 set2)
        (cond
            ((and (null? set1) (null? set2)) '())
            ((null? set1) set2)
            ((null? set2) set1)
            ((= (car set1) (car set2)) (union-iter (cdr set1) set2))
            ((> (car set1) (car set2)) (cons (car set2) (union-iter set1 (cdr set2))))
            ((< (car set1) (car set2)) (cons (car set1) (union-iter (cdr set1) set2)))))
    (list->tree (union-iter (tree->list set1) (tree->list set2))))

(intersection-set test-tree1 test-tree2)
(union-set test-tree1 test-tree2)