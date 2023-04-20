#lang racket

(define (fib n) (fib-iter 1 0 0 1 n))

;; p’ = p^2 + q^2 
;; q’ = q^2 + pq 
(define (fib-iter a b p q count) 
    (cond ((= count 0) b) 
          ((even? count) (fib-iter 
                             a 
                             b 
                             (+ (square p) (square q)) 
                             (+ (square q) (* 2 p q)) 
                  (/ count 2))) 
          (else (fib-iter 
                    (+ (* b q) (* a q) (* a p)) 
                    (+ (* b p) (* a q)) 
                    p 
                    q 
                    (- count 1)))))

(define (square a)
    (* a a))

;; something like tests
;; expected: #t, received: #t
(= 
    (+ (fib 5) (fib 6))
    (fib 7))

;; expected: #t, received: #t
(= 
    (+ (fib 12) (fib 13))
    (fib 14))

;; expected: #t, received: #t
(= 
    (+ (fib 29) (fib 30))
    (fib 31))