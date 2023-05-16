#lang racket

(define tolerance 0.00001)
(define dx 0.00001)

(define (newtons-method g guess) 
    (fixed-point (newton-transform g) guess))

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) 
        (< (abs (- v1 v2)) tolerance)) 
    (define (try guess) 
        (let ((next (f guess))) 
             (if (close-enough? guess next) 
                  next (try next)))) 
    (try first-guess))

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (deriv g) (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

; x -> x^3 + ax^2 + bx + c
(define (cubic a b c)
   (lambda (x) (+ (cube x) 
                  (* a (square x)) 
                  (* b x) 
                  c)))

(define (cube x)
    (* x x x))

(define (square x)
    (* x x))

; a kind of tests
; expected: 0, received: 2.6531990291797187e-5
(newtons-method (cubic 0 0 0) 1)

; expected: 0 or -1, received: 1.1227429100448376e-5
(newtons-method (cubic 1 0 0) 1) 

; expected: approx -1, received: -0.9999999999997796
(newtons-method (cubic 1 1 1) 1)