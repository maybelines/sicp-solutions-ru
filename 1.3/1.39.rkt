#lang racket

(define (tan-cf x k)
    ; modified procedure from 1.37b
    ; removed k from list of arguments (already in tan-cf args)
    ; added x arg after using procedure n because
    ; now sequence of n returns a procedure, not a number
    (define (cont-frac n d)
        (define (iter count res)
            (if (= count 0)
                res
                (iter (- count 1) (/ ((n count) x) (- (d count) res)))))
        (iter k 0))
    (cont-frac n-seq seq-of-odd))

; returns a procedure
(define (n-seq i)
    (if (= i 1)
        (lambda (x) x)
        (lambda (x) (* x x))))

(define (seq-of-odd i)
    (+ (* 2 (- i 1)) 
       1))

; a kind of tests
; expected: 1.55740772465, received: 1.557407724654902
(tan-cf 1.0 10)
; expected: 0.54630248984, received: 0.5463024898437905
(tan-cf 0.5 10)
; expected: 0, received: -1.0189321275162445e-7
(tan-cf pi 10)
