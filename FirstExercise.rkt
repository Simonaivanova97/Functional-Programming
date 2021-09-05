#lang racket
;Задача. Да се напише функция suma, която приема два аргумента и връща техния сбор.
(define (suma x y) (+ x y))

;Задача 1. Да се напише функция mymin, която приема два аргумента и връща по-малкия от тях.
(define (mymin x y)
   (if (< x y)
       x
       y))

;Задача 2. Да се дефинира функцията inside? x a b, която проверява дали числото x се намира в затворения интервал [a, b].
(define (inside x a b)
  (and (>= x a) (<= x b)))

;Задача 3. Да се напише функция myfunc, която пресмята на средно аритметичното от квадратите на 2 числа.
(define (someAvr x y)
  (/ (+ (* x x) (* y y)) 2))

;Задача 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи.
(define (nThFib n)
  (cond [(= n 1) 0]
        [(= n 2) 1]
        [(> n 2) (+ (nThFib (- n 2)) (nThFib (- n 1)))]))

;Сумата на целите числа >= a и <= b.
(define (sum1 a b)
  (if (= a b) b
      (+ a (sum1 (+ a 1) b))))

;Произведението на целите числа >= a и <= b.
(define (prod a b)
  (if (= a b) b
      (* a (prod (+ a 1) b))))

;Факториела на число n.
;1 начин.
(define (fact n)
  (cond [(= n 0) 1]
        [(= n 1) 1]
        [(> n 1) (* n (fact (- n 1)))]))

;Факториела на число n.
;2 начин.
(define (secondFact n)
  (define (factHelper product count)
    (if (> count n ) product
        (factHelper (* product count) (+ count 1))))
  (factHelper 1 1))

;Връща последната цифра на число n.
(define (last-digit n)
  (modulo n 10))

;Връща първата цифра на число n.
(define (first-digit n)
  (if (= (quotient n 10) 0) n
      (first-digit (quotient n 10))))

;Броя на цифрите в едно число
(define (countDigit n)
  (if (< n 10) 1
      (+ 1 (countDigit (quotient n 10)))))

;Да се напише функция mygcd a b, която връща НОД(a, b).
(define (mygcd x y)
  (if (> x y)
      (if (= (remainder x y) 0) y
          (mygcd y (remainder x y)))
  (mygcd y x)))

;Обръща чифрите в едно число: 345 -> 543
(define (degree n) (- (countDigit n) 1))
(define (reverseNumber n)
  (define (helperReverse tempResult tempDegree)
    (if (<= tempResult 0) 0
        (+ (* (remainder tempResult 10) (expt 10 tempDegree)) (helperReverse (quotient tempResult 10) (- tempDegree 1)))))
  (helperReverse n (degree n)))