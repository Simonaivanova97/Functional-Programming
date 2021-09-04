#lang racket
;Задача 2. Да се дефинира предикат (automorphic? n), който приема естествено число n и проверява дали n2 завършва с цифрите
;на n.
(define (lastNNumber number n)
  (define (helper tempNumber tempDegree lastN)
    (if (= tempDegree lastN) 0
        (+ (* (remainder tempNumber 10) (expt 10 tempDegree)) (helper (quotient tempNumber 10) (+ tempDegree 1) lastN))))
  (helper number 0 n))

(define (countDigit n)
  (if (<= n 0) 0
      (+ 1 (countDigit (quotient n 10)))))

(define (automorphic? n)
  (define (square n) (* n n))
  (= n (lastNNumber (square n) (countDigit n))))

;Задача 3. Да се дефинира двуаргументна функция (sum-of-greater-primes n k), която намира сумата на първите n на брой прости
;числа, които са по-големи от k.
(define (isPrimeNumber? n)
  (cond [(= n 1) #f]
        [(> n 1) (define (helperIsPrime n i)
                   (if (= i n) #t
                       (if (= (remainder n i) 0) #f
                           (helperIsPrime n (+ i 1)))))
                 (helperIsPrime n 2)]))

(define (sum-of-greater-primes n k)
  (define (helperFunc beginNumber tempCount maxCount k)
    (if (= tempCount maxCount) 0
        (if (and (isPrimeNumber? beginNumber) (> beginNumber k)) (+ beginNumber (helperFunc (+ beginNumber 1) (+ tempCount 1) maxCount k))
            (helperFunc (+ beginNumber 1) tempCount maxCount k))))
  (helperFunc 1 0 n k))
                                                              