#lang sicp
(define (cube x) (* x x x))

(define (p x)
    (- (* 3 4) (* 4 (cube x))))

(define (sine angle)
    ; (if (not (> (abs angle) 0.1))
    (if (<= (abs angle) 0.1)
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)

a. sine 12.15에서 p가 호출되는 수
5번

p sine 4.05
p p sine 1.35
p p p sine 0.45
p p p p sine 0.15
p p p p p sine 0.05
p p p p p 0.05


b.
sine a의 기억 공간과 계산 단계의 자람 차수를 a의 함수로 나타내면?
a=
0.1 = 1 0
0.3 = 2 1
0.9 = 3 2
2.7 = 4 3
8.1 = 5 4 = 3^(n-1)/4
간단하게 3^n
n=log3 x


