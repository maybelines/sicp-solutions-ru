#lang sicp

;; new prime test module using fast-prime?
(define (fast-prime? n times)
    (cond ((= times 0) true) 
          ((fermat-test n) (fast-prime? n (- times 1))) 
          (else false)))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a)) 
    (define tmp (+ 1 (random (- n 1))))
    (display tmp)
    (try-it tmp))
	

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
    (if (fast-prime? n 1) 
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

; average time with (fast-prime?) is 2 ms, average time with (prime?) is 2 ms
;(search-for-primes 1000 1019)

; average time with (fast-prime?) is 2 ms, average time with (prime?) is 2 ms
;(search-for-primes 100000 100043)

; average time with (fast-prime?) is 2 ms, average time with (prime?) is 6 ms
;(search-for-primes 1000000 1000037)

; average time with (fast-prime?) is 2 ms, average time with (prime?) is 19 ms
;(search-for-primes 10000000 10000103)

; average time with (fast-prime?) is ???????? ms, average time with (prime?) is 63 ms
;!!!(search-for-primes 100000000 100000039)

; average time with (fast-prime?) is 3 ms, average time with (prime?) is 199 ms
;(search-for-primes 1000000000 1000000021)


(define x 100000007)
(define y 1000033)

(fermat-test x)
(fermat-test y)

