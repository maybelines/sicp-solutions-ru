#lang racket

;; generates an iterative process
(define (accumulate combiner null-value term a next b)
    (define (accumulate-iter a result)
        (if (> a b)
            result
            (accumulate-iter (next a)
                             (combiner (term a) 
                                       result))))
    (accumulate-iter a null-value))

(define (sum term a next b)
    (accumulate + 0 term a next b))

(define (product term a next b)
    (accumulate * 1 term a next b))


;; tests for sum
; first test
(define (cube x)
    (* x x x))

(define (inc n) (+ n 1))

(define (sum-cubes a b) 
    (sum cube a inc b))

; expected: 3025, received: 3025
(sum-cubes 1 10) 

; second test
(define (identity x) x)

(define (sum-integers a b) 
    (sum identity a inc b))

; expected: 55, received: 55
(sum-integers 1 10) 

; third test
(define (pi-sum a b)
    (define (pi-term x) 
        (/ 1.0 (* x (+ x 2)))) 
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))

; expected: 3.139592655589783, received: 3.139592655589782
(* 8 (pi-sum 1 1000))


;; test for product
(define (factorial number)
    (if (= number 0)
        1
        (product identity 1 inc number)))

; expected: 1, received: 1
(factorial 0)
; expected: 120, received: 120
(factorial 5)
; expected: 3628800, received: 3628800
(factorial 10)