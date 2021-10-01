#lang sicp
(#%provide (all-defined))
(define (make-table) (list '*table*))


(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup keylist table)
  (if (null? keylist) (cdr table)
      (let ((subtable (assoc (car keylist) (cdr table))))
        (if subtable (lookup (cdr keylist) subtable)
            #f))))

(define (insert! keylist value table)
  (if (null? keylist) (set-cdr! table value)
      (let ((subtable (assoc (car keylist) (cdr table))))
        (if (not subtable) (begin (set! subtable (list (car keylist)))
                                  (set-cdr! table (cons subtable (cdr table)))))
        (insert! (cdr keylist) value subtable)))
  'ok)

(define testtable
  (cons '*table* (cons (cons 'math
                             (cons (cons '+ '43)
                                   (cons (cons '- '45)
                                         (cons (cons '* '42)
                                               '()))))
                       (cons (cons 'letters
                                   (cons (cons 'a '97)
                                         (cons (cons 'b '98)
                                               '())))
                             '()))))

(insert! '(math +) '44 testtable)
(equal? testtable (cons '*table* (cons (cons 'math
                                             (cons (cons '+ '44)
                                                   (cons (cons '- '45)
                                                         (cons (cons '* '42)
                                                               '()))))
                                       (cons (cons 'letters
                                                   (cons (cons 'a '97)
                                                         (cons (cons 'b '98)
                                                               '())))
                                             '()))))
;; => #t

(insert! '(m + - 1 2) 'test testtable)
(equal? testtable (cons '*table*
                        (cons
                         (cons 'm
                               (cons
                                (cons '+
                                      (cons
                                       (cons '-
                                             (cons
                                              (cons 1
                                                    (cons
                                                     (cons 2 'test)
                                                     '()))
                                              '()))
                                       '())) '()))
                         (cons
                          (cons 'math
                                (cons (cons '+ 44)
                                      (cons (cons '- 45)
                                            (cons (cons '* 42)
                                                  '()))))
                          (cons (cons 'letters
                                      (cons (cons 'a 97)
                                            (cons (cons 'b 98)
                                                  '())))
                                '())))))
;; => #t
