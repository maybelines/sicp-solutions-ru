#lang racket

(define number-of-tests 100)

(define (miller-rabin-tests n times)
    (cond ((= times 0) #t)
          ((miller-rabin-single-test n) (miller-rabin-tests n (- times 1)))
          (else #f)))

(define (miller-rabin-single-test n)
    (define (try-it a)
        (= (expmod a (- n 1) n) 1))
    (define y (+ (random (- n 1)) 1))
    (try-it y))

(define (expmod base exp mod)
    (cond ((= exp 0) 1)
          ((even? exp)
           (not-trivial-radical? 
            (expmod base (/ exp 2) mod) mod))
          (else 
           (remainder (* base (expmod base (- exp 1) mod)) 
                              mod))))

(define (not-trivial-radical? number mod)
    (if (and (not (or (= number 1) (= number (- mod 1))))
             (= (remainder (* number number) mod) 1))
        0
        (remainder (* number number) mod)))

(define (square n)
    (* n n))

(miller-rabin-tests 7 number-of-tests)
(miller-rabin-tests 1009 number-of-tests)
(miller-rabin-tests 11 number-of-tests)
(miller-rabin-tests 99 number-of-tests)
(miller-rabin-tests 561 number-of-tests)
(miller-rabin-tests 559 number-of-tests)
