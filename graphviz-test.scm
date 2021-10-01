;; https://github.com/zv/SICP-guile
;; only works with guile. not working in racket.
(define (list->graphviz lst)
  """Convert a list into a set of Graphviz instructions"""
  (define number 0)
  (define result "")
  (define ordinals '())
  (define (result-append! str)
    (set! result (string-append result str)))

  (define* (nodename n #:optional cell)
    (format #f "cons~a~a" n (if cell (string-append ":" cell) "")))

  (define* (build-connector from to #:optional from-cell)
    (format #f "\t~a -> ~a;~%" (nodename from from-cell) (nodename to)))

  (define (build-shape elt)
    (define (build-label cell)
      (cond ((null? cell) "&#x2205;") ; null character
            ((pair? cell) "&#x2022;") ; bullet dot character
            (else (format #f "~a" cell))))
    (set! number (+ number 1))

    (format #f "\t~a [shape=record,label=\"<car> ~a | <cdr> ~a\"];~%"
            (nodename number)
            (build-label (car elt))
            (build-label (cdr elt))))

  (define* (search xs #:optional from-id from-cell)
    (let ((existing (assq xs ordinals)))
      (if (pair? existing) ;; handle lists with cycles
          ;; we've already built a node for this entry, just make a connector
          (result-append! (build-connector from-id (cdr existing) from-cell))
          (begin
            (result-append! (build-shape xs))
            (set! ordinals (assq-set! ordinals xs number))
            (let ((parent-id number))
              ;; make a X->Y connector
              (if (number? from-id)
                  (result-append! (build-connector from-id parent-id from-cell)))
              ;; recurse
              (if (pair? (car xs)) (search (car xs) parent-id "car"))
              (if (pair? (cdr xs)) (search (cdr xs) parent-id "cdr")))))))

  (search lst)
  (string-append "digraph G {\n" result "}\n"))


;; (display (list->graphviz '(1 2 3)))
;; guile graphviz-test.scm | dot -o /dev/stdout -Tpng > diagram.png
(define (list->graphviz-svg g)
  (system (string-append "echo '" (list->graphviz g) "' | dot -Tsvg > graphviz" (strftime "%d-%H-%M-%s" (localtime (current-time))) ".svg"))
  (sleep 1))
