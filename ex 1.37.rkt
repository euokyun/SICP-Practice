#lang sicp
;n d ni값과 di값 k=k마디
(define (cont-frac-1 n d k)
    (define (cont-frac-iter i res)
    (if 
        (= i 0)
        res
        (cont-frac-iter (- i 1) (/ (n i) (+ (d i) res)))
   ))
    (cont-frac-iter k (/ (n k) (d k))) 
)


; 되도는 프로세스(재귀)
(define (cont-frac-2 n d k)
    (if 
        (= k 0)
        (/ (n 1) (d 1)) 
        (/ 
            (n k) 
            (+ 
                (d k)
                (cont-frac-2 n d (- k 1))
            )
        )
   )))


(cont-frac-1 (lambda (i) 1.0) (lambda (i) 1.0) 11)
(cont-frac-2 (lambda (i) 1.0) (lambda (i) 1.0) 11)