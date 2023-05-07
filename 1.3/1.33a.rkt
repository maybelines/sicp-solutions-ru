#lang racket

;; generates a recursive process
(define (filtered-accumulate filter combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (if (filter a) (term a) null-value)
                  (filtered-accumulate filter
                                       combiner 
                                       null-value
                                       term
                                       (next a)
                                       next
                                       b))))

;; miller-rabin test as implementation of prime? predicate
(define number-of-tests 100)

(define (prime? x)
    (miller-rabin-tests x number-of-tests))

(define (miller-rabin-tests n times)
    (cond ((= times 0) #t)
          ((miller-rabin-single-test n) (miller-rabin-tests n (- times 1)))
          (else #f)))

(define (miller-rabin-single-test n)
    (define (try-it a)
        (= (expmod a (- n 1) n) 1))
    (try-it (+ (random (- n 1)) 1)))

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
;; end of prime? implementation

;; implementation of sum of squares prime numbers
(define (inc x)
    (+ x 1))

(define (sum-of-squares-prime-numbers limit)
    (filtered-accumulate prime? + 0 square 2 inc (+ limit 1)))
;; end of implementation

;; implementation of product of coprime naturals less than n 
(define (identity x) x)

(define (product-of-coprime-naturals-less-n n)
    (define (coprime? x)
        (= 1 (gcd x n)))
    (filtered-accumulate coprime? * 1 identity 1 inc n))
;; end of implementation

;; a kind of test
; expected: 38, received: 38
(sum-of-squares-prime-numbers 5)
; expected: 189, received: 189
(product-of-coprime-naturals-less-n 10)