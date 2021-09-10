#lang sicp
(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond 
            [(= trials-remaining 0) (/ trials-passed trials)]
            [(experiment) (iter (- trials-remaining 1) (+ trials-passed 1))]
            [else (iter (- trials-remaining 1) trials-passed)]))
    (iter trials 0))

(define (random-in-range low high)
    (let ([range (- high low)])
        (+ low (random range))))

(define (estimate-integral p x1 y1 x2 y2 trials)
    (* (- x2 x1) (- y2 y1) (monte-carlo trials (p x1 y1 x2 y2))))

(define (p-test x1 y1 x2 y2) 
    (let* ([xc (/ (+ x2 x1) 2)] [yc (/ (+ y2 y1) 2)])
        (lambda () 
            (>= 
                (expt (/ (- x2 x1) 2) 2) 
                (+ 
                    (expt (- (random-in-range x1 x2) xc) 2) 
                    (expt (- (random-in-range y1 y2) yc) 2))))))

(estimate-integral p-test 2.0 4.0 8.0 10.0 100000) ; 28.34352
(estimate-integral p-test 2 4 8 10 100000) ; 27.04788 
; 3^2 pi = 28.27
(- 28.2743 (estimate-integral p-test 2.0 4.0 8.0 10.0 100000))

; pi
(/ (* (monte-carlo 100000 (p-test 2.0 4.0 8.0 10.0)) 36.0) 9.0) 

(random 10) ; 3
(random 10.0) ; 4.250368446595184