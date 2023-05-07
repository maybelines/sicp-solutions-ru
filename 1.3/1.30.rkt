#lang racket

(define (sum term a next b) 
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) 
                  (+ (term a) result))))
    (iter a 0))

;; first test
(define (cube x)
    (* x x x))

(define (inc n) (+ n 1))

(define (sum-cubes a b) 
    (sum cube a inc b))

;; expected: 3025, received: 3025
(sum-cubes 1 10) 

;; second test
(define (identity x) x)

(define (sum-integers a b) 
    (sum identity a inc b))

;; expected: 55, received: 55
(sum-integers 1 10) 

;; third test
(define (pi-sum a b)
    (define (pi-term x) 
        (/ 1.0 (* x (+ x 2)))) 
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))

;; expected: 3.139592655589783, received: 3.139592655589782
(* 8 (pi-sum 1 1000))