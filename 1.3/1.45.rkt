#lang racket

(define tolerance 0.00001)

; procedure from 1.43
(define (repeated f n)
    (define (iter count cur)
        (if (= count 0)
            cur
            (iter (- count 1) 
                  (lambda (x) (f (cur x))))))
    (iter n (lambda (x) x)))

; procedure from 1.40
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) 
        (< (abs (- v1 v2)) tolerance)) 
    (define (try guess) 
        (let ((next (f guess))) 
             (if (close-enough? guess next) 
                  next (try next)))) 
    (try first-guess))

; procedure from average-damping subsection
(define (average-damp f) 
    (lambda (x) (average x (f x))))

(define (average x y)
    (/ (+ x y) 2))

(define (find-n-deg-root x n damp-times)
    (fixed-point ((repeated average-damp damp-times) 
                  (lambda (y) 
                          (/ x (expt y (- n 1)))))
                 1.0))

; experimental results
; for all n in [2, 3] it is sufficient to use average-damp 1 time 
(find-n-deg-root 4 2 1)
(find-n-deg-root 8 3 1) 

; for all n in [4, 7] it is sufficient to use average-damp 2 times 
(find-n-deg-root 16 4 2)
(find-n-deg-root 32 5 2)
(find-n-deg-root 64 6 2)
(find-n-deg-root 128 7 2) 

; for all n in [8, 15] it is sufficient to use average-damp 3 times 
(find-n-deg-root 256 8 3)   
(find-n-deg-root 1024 10 3)
(find-n-deg-root 32768 15 3)

; for all n in [16, 39] it is sufficient to use average-damp 4 time 
(find-n-deg-root 65536 16 4) 
(find-n-deg-root 262144 18 4)
(find-n-deg-root 1048576 20 4)
; but actually
(find-n-deg-root 536870912 29 3)
(find-n-deg-root 549755813888 39 3)

; the following values of n require at least 5 usage
(find-n-deg-root 1099511627776 40 5)
