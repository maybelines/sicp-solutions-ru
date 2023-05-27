#lang racket

;; procedural implementation of natural numbers

; the implementation of integer 0 as procedure
(define zero 
    (lambda (f) (lambda (x) x)))

; the implementation of add integer 1 to procedural natural number
(define (add-1 n) 
    (lambda (f) 
        (lambda (x) (f ((n f) x)))))

; based on the previous two implementations, it is easy to understand that:
(define one
    (lambda (f) (lambda (x) (f x))))

(define two
    (lambda (f) (lambda (x) (f (f x)))))

; the plus procedure in these terms means a kind of compose procedure
; alike as add-1
(define (plus n1 n2)
    (lambda (f)
        (lambda (x) ((n1 f) ((n2 f) x)))))

; ----------------------------------------------
; a kind of tests
(define (inc x)
    (+ x 1))

; expected: 0, recieved: 0
(((plus zero zero) inc) 0)
; expected: 2, recieved: 2
(((plus two zero) inc) 0)
; expected: 3, recieved: 3
(((plus one two) inc) 0)