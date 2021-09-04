#lang racket
; Задача 1. Да се дефинира функцията (perfect-number? n), която проверява дали числото n e съвършено, т.е. дали е равно на сбора на
; делителите си.
(define (sumOfDivisors n)
  (define (helper number i)
    (if (= i number) 0
        (if (= (remainder number i) 0) (+ i (helper number (+ i 1)))
            (helper number (+ i 1)))))
  (helper n 1))

(define (perfect-number? n)
  (if (= n 0) #f
      (= n (sumOfDivisors n))))

; Задача 2. Да се дефинира функцията (inc-digits? n), която проверява дали цифрите на числто n са подредени в нарастващ ред.
(define (biggerThanAll? number n)
  (if (= number 0) #t
      (if (< n (remainder number 10)) #f
          (biggerThanAll? (quotient number 10) n))))

(define (inc-digits? n)
  (if (= n 0) #t
      (if (not (biggerThanAll? (quotient n 10) (remainder n 10))) #f
          (inc-digits? (quotient n 10)))))

;Задача 3. По зададени x и n, да се изчисли сумата: 1 + x + x^2 + x^3 + ... + x^n.
(define (findSum x n)
  (define (helper number tempN MaxN)
    (if (< tempN 0) 0
        (+ (expt number tempN) (helper number (- tempN 1) MaxN))))
  (helper x n n))

;(to-binary 6) -> 110
(define (to-binary number)
  (define (helper n tempDegree)
    (if (= (quotient n 2) 0) (* (remainder n 2) (expt 10 tempDegree))
        (+ (* (remainder n 2) (expt 10 tempDegree)) (helper (quotient n 2) (+ tempDegree 1)))))
  (helper number 0))