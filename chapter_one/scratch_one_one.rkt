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

; getting a square root

(define (new-if predicate
                then-clause
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (average x y)
  (/ (+ x y ) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt-iter-b guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1 x))

(define (sqrt-b x)
  (sqrt-iter-b 1 x))
