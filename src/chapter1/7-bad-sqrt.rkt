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

;; expected: 0.000000005, received: 0.031250000000000264
(sqrt 0.000000000000000025)

;; expected: 10^{26}, received: no response
;; (sqrt 10000000000000000000000000000000000000000000000000000)