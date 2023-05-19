#lang racket

(define (iterative-improve is-good-enough? improve)
    (define (iter x)
        (if (is-good-enough? x)
            x
            (iter (improve x))))
    (lambda (x) (iter x)))

(define (sqrt x)
    (define tolerance 0.00001)
    (define (average y1 y2) (/ (+ y1 y2) 2))
    (define (get-better-val y) (average y (/ x y)))
    (define (is-close? y)
        (< (abs (- (get-better-val y)
                   y))
           tolerance))
    ((iterative-improve is-close? get-better-val) 1.0))

; something like tests for sqrt
; expected: 5, received: 5.000000000053722
(sqrt 25)
; expected: 13, received: 13.000000070110696
(sqrt 169)

(define (fixed-point f)
    (define tolerance 0.00001)
    (define (is-close? x)
        (< (abs (- (f x) x))
           tolerance))
    ((iterative-improve is-close? (lambda (x) (f x))) 1.0))

; something like tests for fixed-point
; expected: 0.7390822985224023, received: 0.7390893414033928
(fixed-point cos)
; expected: 1.2587315962971173, received: 1.2587228743052672
(fixed-point (lambda (y) (+ (sin y) (cos y))))
