#lang racket

(define (div-interval x y)
        (if (is-end-of-interval-zero? y)
            (raise-arguments-error 'div-interval
                                   "one of the ending points of interval is zero")
            (mul-interval x (inverse-by-mul-interval y))))

; ----------abstraction-barrier------------

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y))) 
          (p2 (* (lower-bound x) (upper-bound y))) 
          (p3 (* (upper-bound x) (lower-bound y))) 
          (p4 (* (upper-bound x) (upper-bound y)))) 
        (make-interval (min p1 p2 p3 p4) 
                       (max p1 p2 p3 p4))))

(define (is-end-of-interval-zero? x)
    (if (or (= (lower-bound x) 0) 
            (= (upper-bound x) 0))
        #t
        #f))

(define (inverse-by-mul-interval x)
    (make-interval (/ 1.0 (upper-bound x)) 
                   (/ 1.0 (lower-bound x))))

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

(define alpha (make-interval 1 3))
(define beta (make-interval 0 2))
(define gamma (make-interval 2 4))

; expected: "one of the ending points of interval is zero"
; received: "one of the ending points of interval is zero"
(with-handlers ([exn:fail?
                    (lambda (exn) ((error-display-handler) (exn-message exn) exn))])
    (div-interval alpha beta))

; expected: (0.25, 1.5), received: (0.25, 1.5)
(print-interval (div-interval alpha gamma))