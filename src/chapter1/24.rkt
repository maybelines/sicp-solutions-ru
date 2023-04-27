#lang sicp

(define number-of-tests 100)
(define random-limit 4294967087)

;; new prime test module using fast-prime?
(define (fast-prime? n times)
    (cond ((= times 0) true) 
          ((fermat-test n) (fast-prime? n (- times 1))) 
          (else false)))

(define (fermat-test n)
    (define (try-it a) 
        (= (expmod a n n) a)) 
   (try-it (+ 1 (random (min (- n 1) random-limit)))))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) 
           (remainder (square (expmod base (/ exp 2) m)) 
                      m)) 
          (else 
           (remainder (* base (expmod base (- exp 1) m)) 
                      m))))

(define (square n)
    (* n n))
;; end of new prime test module

;; time module
(define (timed-prime-test n) 
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (fast-prime? n number-of-tests) 
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

; average time is 400 ms
(search-for-primes 10000000000 10000000061)

; average time is 1218 ms
(search-for-primes 100000000000000000000 100000000000000000151)