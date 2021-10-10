#lang sicp
(#%provide (all-defined))
(#%require "3.3.4.rkt")
;; (#%require "ex3.28.rkt")


(define (ripple-carry-adder alist blist slist c)
  (if (null? alist) 'ok
      (let ((c-out (make-wire)))
        (full-adder (car alist) (car blist) c (car slist) c-out)
        (ripple-carry-adder (cdr alist) (cdr blist) (cdr slist) c-out)
        )))
