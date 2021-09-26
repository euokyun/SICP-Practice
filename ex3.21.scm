(load "3.3.2.scm")
(load "graphviz-test.scm")

(define q1 (make-queue))

(insert-queue! q1 'a)
q1
;; => ((a) a)
;; (list->graphviz-svg q1)

(insert-queue! q1 'b)
q1
;; => ((a b) b)
;; (list->graphviz-svg q1)

(delete-queue! q1)
q1
;; => ((b) b)
;; (list->graphviz-svg q1)

(delete-queue! q1)
q1
;; => (() b)
;; (list->graphviz-svg q1)


(define print-queue front-ptr)
