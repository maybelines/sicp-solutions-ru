#lang racket

(define (even? n) 
    (= (remainder n 2) 0))

(define (double a)
    (* 2 a))

(define (halve a)
    (/ a 2))

(define (fast-mul-iter tmp a b)
    (cond ((= b 1) (+ tmp a))
          ((even? b) (fast-mul-iter
                         tmp 
                         (double a) 
                         (halve b)))
          (else (fast-mul-iter (+ tmp a) a (- b 1)))))

(define (fast-mul a b)
    (fast-mul-iter 0 a b))

;; something like tests
;; expected: 25, received: 25
(fast-mul 5 5)

;; expected: 72, received: 72 
(fast-mul 9 8)

;; expected: 1824, received: 1824
(fast-mul 32 57)