#lang sicp

;; prime test module 
(define (smallest-divisor n) 
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n) 
          ((divides? test-divisor n) test-divisor) 
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) 
    (= (remainder b a) 0))

(define (prime? n) 
    (= n (smallest-divisor n)))

(define (square n)
    (* n n))
;; end of prime test module

;; time module
(define (timed-prime-test n) 
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (prime? n) 
        (report-prime n (- (runtime) start-time))
        0))

(define (report-prime n elapsed-time) 
    (display n)
    (display " *** ")
    (display elapsed-time)
    (newline))
;; end of time module

(define (search-for-primes start end)
    (cond ((= (remainder start 2) 0) 
           (search-for-primes (+ start 1) end))
          ((> start end) (newline))
          (else 
           (timed-prime-test start)
           (search-for-primes (+ start 2) end)))) 

; average time is 2 ms
(search-for-primes 100000 100043)

; average time is 6 ms
(search-for-primes 1000000 1000037)

; average time is 19 ms
(search-for-primes 10000000 10000103)

; average time is 63 ms
(search-for-primes 100000000 100000039)

; average time is 199 ms
(search-for-primes 1000000000 1000000021)


