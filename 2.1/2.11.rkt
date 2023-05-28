#lang racket

;; in my opinion Ben's notice incorrect, the version with nine cases
;; only one of which requires more than two multiplications is not exist

;; show this:
;; the first example is case (x1, x2) * (y1, y2) where sign(x1 * x2) = -1 and sign(y1 * y2) = -1
;; (-1000, 1) * (-1000, 2) = (x1 * y2, x1 * y1) = (-2000, 1000000)
;; (-10, 10) * (-10, 100) =  (x1 * y2, x2 * y2) = (-1000, 1000)
;; (-1, 1) * (-2, 1) =       (x2 * y1, x1 * y1) = (-2, 2)
;; so it means requires at least 3 multiplies

;; the second example is case (x1, x2) * (y1, y2), where sign(x1 * x2) = 1 and sign(y1 * y2) = 1
;; (-10, -2) * (-100, -3) = (x2 * y2, x1 * y1) = (6, 1000)
;; (-10, -2) * (2, 10) =    (x1 * y2, x2 * y1) = (-100, -4)
;; (1, 2) * (2, 3) =        (x1 * y1, x2 * y2) = (2, 6)
;; so it means requires at least 3 multiplies

;; below is my modest method of reducing the number of multiplications
;; only in three cases we have to use the old procedure
;; in all other cases we use 4 multiplications (excluding muls in the condition)

(define (mul-interval x y)
    (let ((x1 (lower-bound x)) 
          (x2 (upper-bound x)) 
          (y1 (lower-bound y)) 
          (y2 (upper-bound y)))
    (if (< (+ (sign (* x1 x2)) 
              (sign (* y1 y2))) 
           1)
        (make-interval (min (* x1 y2) (* x2 y1)) 
                       (max (* x1 y1) (* x2 y2)))
        (mul-interval-old x1 x2 y1 y2))))

(define (mul-interval-old x1 x2 y1 y2)
    (let ((p1 (* x1 y1)) 
          (p2 (* x1 y2)) 
          (p3 (* x2 y1)) 
          (p4 (* x2 y2))) 
        (make-interval (min p1 p2 p3 p4) 
                       (max p1 p2 p3 p4))))

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

(define (sign x)
    (cond ((< x 0) -1)
          ((= x 0) 0)
          ((> x 0) 1)))

; -----------------------------------------
; a kind of tests
(define a (make-interval -1000 1))
(define b (make-interval -1000 2))
(define a1 (make-interval -10 10))
(define b1 (make-interval -10 100))
(define a2 (make-interval -1 1))
(define b2 (make-interval -2 1))

;; (-1000, 1) * (-1000, 2) = (x1 * y2, x1 * y1) = (-2000, 1000000)
;; (-10, 10) * (-10, 100) =  (x1 * y2, x2 * y2) = (-1000, 1000)
;; (-1, 1) * (-2, 1) =       (x2 * y1, x1 * y1) = (-2, 2)
(print-interval (mul-interval a b))
(print-interval (mul-interval a1 b1))
(print-interval (mul-interval a2 b2))

(define x (make-interval -10 -2))
(define y (make-interval -100 -3))
(define z (make-interval 2 10))
(define p (make-interval 1 2))
(define q (make-interval 2 3))

;; (-10, -2) * (-100, -3) = (x2 * y2, x1 * y1) = (6, 1000)
;; (-10, -2) * (2, 10) =    (x1 * y2, x2 * y1) = (-100, -4)
;; (1, 2) * (2, 3) =        (x1 * y1, x2 * y2) = (2, 6)
(print-interval (mul-interval x y))
(print-interval (mul-interval x z))
(print-interval (mul-interval p q))