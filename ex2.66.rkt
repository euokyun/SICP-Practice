#lang sicp
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define key #t) 
(define (lookup given-key set-of-records)
    (cond
        ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
            (entry set-of-records))
        ((> given-key (key (entry set-of-records))) 
            (lookup given-key (right-branch set-of-records)))
        ((< given-key (key (entry set-of-records))) 
            (lookup given-key (left-branch set-of-records)))))