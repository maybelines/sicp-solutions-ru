#lang racket

(define (sqrt-iter last-guess new-guess x)
    (if (little-changes? last-guess new-guess) 
        new-guess 
        (sqrt-iter new-guess 
                   (improve new-guess x) 
                   x)))

(define (improve guess x) 
    (average guess (/ x guess)))

(define (average x y) 
    (/ (+ x y) 2))

(define (little-changes? last-guess new-guess)
    (< (abs (- new-guess last-guess)) 
       (* 0.001 new-guess)))

(define (sqrt x) 
    (sqrt-iter 0.0 1.0 x))

;; процедура вычисления квадратного корня действительно лучше работает с 
;; маленькими и большими числами с проверкой завершения little-changes?
;; для примера можно рассмотреть следующие два вызова процедуры
(sqrt 0.000000000000000025)
(sqrt 10000000000000000000000000000000000000000000000000000)