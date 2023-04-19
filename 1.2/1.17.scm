#lang racket

(define (double a)
    (* 2 a))

(define (halve a)
    (/ a 2))

(define (even? n) 
    (= (remainder n 2) 0))

(define (fast-mul a b)
    (cond ((= b 1) a) 
          ((even? b) (fast-mul 
                         (double a) 
                         (halve b)))
          (else (+ a (fast-mul a (- b 1))))))

;; something like tests
;; expected: 25, received: 25
(fast-mul 5 5)

;; expected: 72, received: 72 
(fast-mul 9 8)

;; expected: 1824, received: 1824
(fast-mul 32 57)