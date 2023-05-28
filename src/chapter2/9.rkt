#lang racket

(define (width-of-interval interval)
    (/ (- (upper-bound interval) (lower-bound interval)) 2.0))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y)) 
                   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval interval1 interval2)
    (make-interval (- (lower-bound interval1) (lower-bound interval2))
                   (- (upper-bound interval1) (upper-bound interval2))))

; actually this procedure violates "Single Level of Abstraction" principle :)
(define (div-interval x y)
    (mul-interval x 
        (make-interval (/ 1.0 (upper-bound y)) 
                       (/ 1.0 (lower-bound y)))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y))) 
          (p2 (* (lower-bound x) (upper-bound y))) 
          (p3 (* (upper-bound x) (lower-bound y))) 
          (p4 (* (upper-bound x) (upper-bound y)))) 
        (make-interval (min p1 p2 p3 p4) 
                       (max p1 p2 p3 p4))))

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
(define alpha (make-interval 0 3))
(define beta (make-interval 1 4))
(define gamma (make-interval -1 2))
(define beta-x (make-interval 1 0.25))
(define gamma-x (make-interval -1 0.5))

; expected: 1.5, received: 1.5
(width-of-interval alpha)
; expected: 1.5, received: 1.5
(width-of-interval beta)
; expected: 1.5, received: 1.5
(width-of-interval gamma)

; expected: 6, received: 6
(width-of-interval (mul-interval alpha beta))
; expected: 4.5, received: 4.5
(width-of-interval (mul-interval alpha gamma))

; expected: 6, received: 6
(width-of-interval (div-interval alpha beta-x))
; expected: 4.5, received: 4.5
(width-of-interval (div-interval alpha gamma-x))