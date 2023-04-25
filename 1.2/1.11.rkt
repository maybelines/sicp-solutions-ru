#lang racket

;; f(n) = n, if n < 3
;; f(n) = f(n−1) + f(n−2) + f(n−3), if n ≥ 3

(define (f-recursive n)
    (if (< n 3)
        n
        (+ (f-recursive (- n 1)) 
            (f-recursive (- n 2))
            (f-recursive (- n 3)))))

(define (f-iterative n)
    (f-iterative-round 0 1 2 n))

(define (f-iterative-round x1 x2 x3 n)
    (cond ((= n 0) x1)
          ((= n 1) x2)
          ((= n 2) x3)
          (else (f-iterative-round x2 x3 (+ x1 x2 x3) (- n 1)))))

;; something like tests
;; expected: #t, received: #t
(= (f-recursive 5) 11)

;; expected: #t, received: #t
(= (f-iterative 5) 11)

;; expected: #t, received: #t
(= (f-recursive 10) (f-iterative 10))