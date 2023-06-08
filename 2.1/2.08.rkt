#lang racket

; this exercise allows us to understand that the set of intervals
; with the addition operation does not form a group

(define (sub-interval interval1 interval2)
    (add-interval interval1 
                  (inverse-by-add interval2)))

; ----------abstraction-barrier------------

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y)) 
                   (+ (upper-bound x) (upper-bound y))))

(define (inverse-by-add interval)
    (make-interval (- 0 (upper-bound interval))
                   (- 0 (lower-bound interval))))

(define (print-interval x) 
    (newline) 
    (display "(") 
    (display (lower-bound x)) 
    (display ", ") 
    (display (upper-bound x)) 
    (display ")"))

; ----------abstraction-barrier------------

(define (make-interval a b) 
    (if (< a b)
        (cons a b)
        (cons b a)))

(define (lower-bound interval)
    (car interval))

(define (upper-bound interval)
    (cdr interval))

; ----------abstraction-barrier------------
; a kind of tests

(define a (make-interval 1 5))
(define b (make-interval 0 3))
(define c (make-interval -5 -1))

; expected: (-2, 5), received: (-2, 5)
(print-interval (sub-interval a b))
; expected: (1, 8), received: (1, 8)
(print-interval (sub-interval b c))
; expected: (2, 10), received: (2, 10)
(print-interval (sub-interval a c))


