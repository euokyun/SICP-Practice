#lang sicp
(load "graphviz-test.scm")

(define (make-deque) (cons '() '()))
(define front-deque car)
(define rear-deque cdr)
(define set-front-deque! set-car!)
(define set-rear-deque! set-cdr!)

(define next-deque-item cadr)
(define prev-deque-item caddr)
(define (set-next-deque-item! side-deque item)
  (set-car! (cdr side-deque) item))
(define (set-prev-deque-item! side-deque item)
  (set-car! (cddr side-deque) item))
(define (empty-deque? deque) (null? (front-deque deque)))

(define (print-deque deque)
  (define (print-deque-1 front)
    (if (null? front) '()
        (cons (car front) (print-deque-1 (next-deque-item front)))))
  (print-deque-1 (front-deque deque)))

;; (define (rear-insert-deque! deque item)
;;   (let ((new-pair (cons item '())))
;;     (cond ((empty-deque? deque)
;;            (set-front-deque! deque new-pair) (set-rear-deque! deque new-pair)
;;            deque)
;;           ;; ((null? rear-deque) (set-car! rear-deque item)) ;; after rear-delete
;;           (else (set-rear-deque! (rear-deque deque) new-pair)
;;                 (set-rear-deque! deque new-pair)
;;                 deque))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (list item '() (rear-deque deque))))
    (cond ((empty-deque? deque)
           (set-front-deque! deque new-pair) (set-rear-deque! deque new-pair)
           ;; deque
           (print-deque deque))
          (else (set-car! (cdr (rear-deque deque)) new-pair)
                (set-rear-deque! deque new-pair)
                (print-deque deque)))))

;; (define (front-delete-deque! deque)
;;   (cond ((empty-deque? deque)
;;          (error "FRONT-DELETE called with an empty deque" deque))
;;         (else (set-front-deque! deque (cdr (front-deque deque)))
;;               deque)))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE called with an empty deque" deque))
        ;; ((null? (next-deque-item (front-deque deque)))
        ;;  (set! deque (make-deque)) deque)
        (else (set-front-deque! deque (next-deque-item (front-deque deque)))
              (if (empty-deque? deque) (set-rear-deque! deque '())
                  (set-prev-deque-item! (front-deque deque) '()));; empty deque.
              ;; (set-cdr! (cdr (front-deque deque)) '())
              (print-deque deque))))

;; (define (front-insert-deque! deque item)
;;   (let ((new-pair (cons item '())))
;;     (cond ((empty-deque? deque)
;;            (set-front-deque! deque new-pair) (set-rear-deque! deque new-pair)
;;            deque)
;;           (else (set-cdr! new-pair (front-deque deque))
;;                 (set-front-deque! queue new-pair)
;;                 queue))))
(define (front-insert-deque! deque item)
  (let ((new-pair (list item (front-deque deque) '())))
    (cond ((empty-deque? deque)
           (set-front-deque! deque new-pair) (set-rear-deque! deque new-pair)
           (print-deque deque))
          (else (set-prev-deque-item! (front-deque deque) new-pair)
                (set-front-deque! deque new-pair)
                (print-deque deque)))))

;; (define (rear-delete-deque! deque)
;;   (cond ((empty-deque? deque)
;;          (error "REAR-DELETE called with an empty deque" deque))
;;         (else (set-front-deque! deque ((front-ptr deque)))
;;               deque)))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE called with an empty deque" deque))
        (else (set-rear-deque! deque (prev-deque-item (rear-deque deque)))
              (if (null? (rear-deque deque)) (set-front-deque! deque '())
                  (set-next-deque-item! (rear-deque deque) '()))
              (print-deque deque))))

(define deque (make-deque))
(rear-insert-deque! deque 'a)
;; => ((a () ()) a () ())
(rear-insert-deque! deque 'b)
;; => ((a (b () #-4#) ()) b () (a #-4# ()))
(rear-insert-deque! deque 'c)
;; => ((a (b (c () #-4#) #-4#) ()) c () (b #-4# (a #-4# ())))
(rear-insert-deque! deque 'd)
;; => ((a (b (c (d () #-4#) #-4#) #-4#) ()) d () (c #-4# (b #-4# (a #-4# ()))))
(front-delete-deque! deque)
;; => ((b (c (d () #-4#) #-4#) ()) d () (c #-4# (b #-4# ())))
(front-insert-deque! deque 'a)
;; => ((a (b (c (d () #-4#) #-4#) #-4#) ()) d () (c #-4# (b #-4# (a #-4# ()))))
(rear-delete-deque! deque)
;; => ((a (b (c () #-4#) #-4#) ()) c () (b #-4# (a #-4# ())))