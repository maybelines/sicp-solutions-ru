#lang racket

(define (sqrt-iter guess x)
    (if (good-enough? guess x) 
        guess 
        (sqrt-iter (improve guess x) 
                   x)))

(define (improve guess x) 
    (average guess (/ x guess)))

(define (average x y) 
    (/ (+ x y) 2))

(define (square x)
    (* x x))

(define (good-enough? guess x) 
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) 
    (sqrt-iter 1.0 x))

;; процедура вычисления квадратного корня не очень хорошо работает с 
;; маленькими и большими числами с проверкой завершения good-enough?
;; для примера можно рассмотреть следующие два вызова процедуры (бесконечный цикл!)
(sqrt 0.000000000000000025)
(sqrt 10000000000000000000000000000000000000000000000000000)