#lang racket

(define e 2.71828182846)
(define k 10)

; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, . . .
(define (generate-seq i)
    (if (= (remainder (- i 2) 3) 0)
        (+ 2 
           (* (/ (- i 2) 3)
              2))
        1))

; procedure from 1.37b
(define (cont-frac n d k)
    (define (iter count res)
        (if (= count 0)
            res
            (iter (- count 1) (/ (n count) (+ (d count) res)))))
    (iter k 0))

; approximation for e
(+ 2 (cont-frac (lambda (i) 1.0) generate-seq k))