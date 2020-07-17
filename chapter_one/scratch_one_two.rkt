(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (fact-iter product counter max-count)
    (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial-two n)
     (fact-iter 1 1 n))

;counting change

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         0)
        (else
          (+ (cc amount (- kinds-of-coins 1))
             (cc (- amount (first-denomination
                             kinds-of-coins))
                 kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Exponentiation

; Linear recursive 𝚯(n) steps and space
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

; linear iteration which requires 𝚯(n) steps but 𝚯(1) space

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product))))

; improving the algorithm allows us to achieve 𝚯(log(n)) steps and space
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;Euclid's Algorithm

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

; Finding prime numbers
; Using Fermats theorem a number n is likely prime if
; a is a random integral number < n
; a^n modulo n == n
; The more iterations confirm this the more likely the number is prime
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))
