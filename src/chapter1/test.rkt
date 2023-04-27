#lang sicp

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) 
           (remainder (square (expmod base (/ exp 2) m)) 
                      m)) 
          (else 
           (remainder (* base (expmod base (- exp 1) m)) 
                      m))))

(define (timed-prime-test) 
    (start-prime-test (runtime)))

(define (timed-prime-test-1) 
    (start-prime-test-1 (runtime)))

(define (start-prime-test start-time)
    (* 1123456781911234567819 10000000019)
    (report-prime (- (runtime) start-time)))

(define (start-prime-test-1 start-time)
    (* 100000000000000000039100000000000000000039 100000000000000000039)
    (report-prime (- (runtime) start-time)))

(define (report-prime elapsed-time) 
    (display " *** ")
    (display elapsed-time)
    (newline))

(define (square n)
    (* n n))


;10000000019 *** 376
;10000000033 *** 371
;10000000061 *** 378

;100000000000000000039 *** 1223
;100000000000000000129 *** 1164
;100000000000000000151 *** 1174

(timed-prime-test)
(timed-prime-test-1)
