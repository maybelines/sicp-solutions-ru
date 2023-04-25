#lang racket

(define (even? n) 
    (= (remainder n 2) 0))

(define (square a)
    (* a a))

(define (fast-expt b n)
    (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter 
                         a 
                         (square b) 
                         (/ n 2))) 
          (else (fast-expt-iter 
                    (* a b) 
                    b 
                    (- n 1)))))


;; something like tests
;; expected: 1024, received: 1024 
(fast-expt 2 10)

;; expected: 19683, received: 19683
(fast-expt 3 9)

;; expected: 15625, received: 15625
(fast-expt 5 6)

