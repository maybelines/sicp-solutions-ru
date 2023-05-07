#lang racket

;; generates a recursive process
(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))

;; implementation of factorial
(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial number)
    (if (= number 0)
        1
        (product identity 1 inc number)))

;; a kind of tests
; expected: 1, received: 1
(factorial 0)
; expected: 120, received: 120
(factorial 5)
; expected: 3628800, received: 3628800
(factorial 10)

;; implementation of pi approximation

; 2 · 4 · 4 · 6 · 6 · 8
; 3 · 3 · 5 · 5 · 7 · 7
(define (fraction x)
    (if (even? x)
        (/ x (+ x 1))
        (/ (+ x 1) x)))

(define (pi-approximation n)
    (* 4.0 (product fraction 2 inc (+ n 3))))

;; a kind of tests
(pi-approximation 10)
(pi-approximation 100)
(pi-approximation 1000)