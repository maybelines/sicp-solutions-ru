#lang racket

(define (cube-root-iter guess x)
    (if (good-enough? guess x) 
        guess 
        (cube-root-iter (improve x guess) 
                        x)))

(define (improve x y) 
    (/ (+ (/ x (square y)) 
          (* 2 y)) 
       3))

(define (square x)
    (* x x))

(define (cube x)
    (* x x x))

(define (good-enough? guess x) 
    (< (abs (- (cube guess) x)) 0.001))

(define (cube-root x) 
    (cube-root-iter 1.0 x))

;; some kind of tests below:
;; expected: 2, received: 2.000004911675504
(cube-root 8)

;; expected: -3, received: -3.000000005383821
(cube-root -27)

;; 154854153 = 537^3 
;; expected: 537, received: 537.0
(cube-root 154854153)
