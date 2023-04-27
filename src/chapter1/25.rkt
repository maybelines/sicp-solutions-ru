#lang sicp

(define number-of-tests 100)
(define random-limit 4294967087)

;; fast-expt module
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

(define (square n)  
    (* n n)) 
;; end of fast-expt module

;; prime test module
(define (fast-prime? n times)
    (cond ((= times 0) true) 
          ((fermat-test n) (fast-prime? n (- times 1))) 
          (else false)))

(define (fermat-test n)
    (define (try-it a) 
        (= (expmod a n n) a)) 
   (try-it (+ 1 (random (min (- n 1) random-limit)))))

(define (expmod base exp m) 
    (remainder (fast-expt base exp) m))
;; end of prime test module

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

; average time is 7250 ms, average time with normal expmod is 40 ms
(search-for-primes 1000 1019)