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
