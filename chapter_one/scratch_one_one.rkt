; Writing out the code from Chapter 1.1

(define pi 3.14159)

(define radius 10)

(define circumference (* 2 pi radius))

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
        ((< x 0) (- x))
        ((= x 0) x)))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs3 x)
  (if (< x 0)
    (- x)
    x))

(define (>= x y)
  (or (> x y) (= x y)))

(define (gte x y)
  (not (< x y)))
