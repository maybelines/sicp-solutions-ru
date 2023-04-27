#lang racket

(define number-of-tests 100)

(define (fermat-tests n times)
    (cond ((> number-of-tests 0) #t)
          ((fermat-single-test n)
           (- times 1))
          (else #f)))

(define (fermat-single-test n)
    (define (try-it a n)
        (= (expmod a n n) a))
    (try-it (+ (random (- n 1)) 1) n))
        
(define (expmod base exp mod)
    (cond ((= exp 0) 1) 
          ((even? exp) (expmod base (/ exp 2) mod))
          (else (expmod base (- exp 1) mod))))

; Сarmichael numbers: 561, 1105, 1729, 2465, 2821, 6601
(fermat-tests 561 number-of-tests)
(fermat-tests 1105 number-of-tests)
(fermat-tests 1729 number-of-tests)
(fermat-tests 2465 number-of-tests)
(fermat-tests 2821 number-of-tests)
(fermat-tests 6601 number-of-tests)