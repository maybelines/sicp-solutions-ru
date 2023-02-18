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

;; expected: 0.000000005 = 5e-9, received: 5.000000004716252e-9
(sqrt 0.000000000000000025)

;; expected: 10^{26} = 1e+26, received: 1.0000000000353524e+26
(sqrt 10000000000000000000000000000000000000000000000000000)