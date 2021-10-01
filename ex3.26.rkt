#lang sicp

(define key car)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (is>? a b)
  (if (and (symbol? a) (symbol? b)) (string>? (symbol->string a) (symbol->string b))
      (> a b)))
(define (is<? a b)
  (if (and (symbol? a) (symbol? b)) (string<? (symbol->string a) (symbol->string b))
      (< a b)))

;; - > + > * > !
(define testrecords
  (list (cons '+ 43) (list (cons '* 42) (list (cons '! 42) '() '()) '()) (list (cons '- 45) '() '())))

(define (lookup given-key set-of-records)
  (cond
    ((null? set-of-records) false)
    ((equal? given-key (key (entry set-of-records)))
     (entry set-of-records))
    ((is>? given-key (key (entry set-of-records)))
     (lookup given-key (right-branch set-of-records)))
    ((is<? given-key (key (entry set-of-records)))
     (lookup given-key (left-branch set-of-records)))))

(equal? (lookup '+ testrecords) (cons '+ 43))
;; => #t
(equal? (lookup '- testrecords) (cons '- 45))
;; => #t
(equal? (lookup '* testrecords) (cons '* 42))
;; => #t
(equal? (lookup '! testrecords) (cons '! 42))
;; => #t
