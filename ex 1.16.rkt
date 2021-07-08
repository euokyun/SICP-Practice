#lang sicp
(define (fe-rec b n)
    (define (fe-iter b n rtn)
        (cond 
            ((= n 0) rtn)
            ((even? n) (fe-iter (square b) (/ n 2) rtn))
            (else (fe-iter b (- n 1) (* b rtn)))
        )
    )
    (fe-iter b n 1)
)
; f 2 10 1 ; 짝수
; f 4 5 1 ; 홀수
; f 4 4 4 ; 짝수
; f 16 2 4 ; 짝
; f 256 1 4 ; 홀
; f 256 0 1024

; =1024