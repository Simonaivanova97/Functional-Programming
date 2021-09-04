#lang racket
;Задача 1. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1, за който d < x.
(define (mymaxdivisor x)
  (define (helperMaxDivisor number i)
    (if (= (remainder number i) 0) i
        (helperMaxDivisor number (- i 1))))
  (helperMaxDivisor x (- x 1)))

;Задача 2. Да се дефинира функцията leap-year? year, която проверява дали годината year е високосна.
(define (leap-year year)
  (if (= (remainder year 4) 0)
      (if (= (remainder year 100) 0)
          (if (= (remainder year 400) 0) #t
              #f)
          #t)
      #f))

;Задача 4. Да се дефинира функция, която намира сумата на нечетните числа в затворения интервал [a, b].
(define (sumOddNumbers a b)
  (if (= a b)
      (if (not (= (remainder b 2) 0))
          b
          0)
      (if (not (= (remainder a 2) 0))
          (+ a (sumOddNumbers (+ a 1) b))
          (+ 0 (sumOddNumbers (+ a 1) b)))))

;Задача 5. Да се дефинира предикат, който проверява дали естественото число n е просто.
(define (helper number i)
   (if (= i 1) #t
       (if (= (remainder number i) 0) #f
           (helper number (- i 1)))))

(define (primeNumber n)
  (if (= n 1) #f
  (helper n (- n 1))))

;Задача 6. Да се дефинира функция, която намира броя на палиндромите в интервала [a, b], където a и b са цели неотрицателни числа и a<b.
(define (countDigitNumber n)
  (if (= n 0) 0
      (+ 1 (countDigitNumber (quotient n 10)))))

(define (degree n) (- (countDigitNumber  n) 1))

(define (reverseNumber n)
  (define (helperReverse tempNumber tempDegree)
    (if (< tempDegree 0) 0
      (+ (* (remainder tempNumber 10) (expt 10 tempDegree)) (helperReverse (quotient tempNumber 10) (- tempDegree 1)))))
  (helperReverse n (degree n)))

(define (countPolindrome a b)
  (if (> a b) 0
      (if (= a (reverseNumber a)) (+ 1 (countPolindrome (+ a 1) b))
          (countPolindrome (+ a 1) b))))

;Задача 7. Да се дефинира функция, която чрез линейно итеративен процес намира броя на естествените делители на едно естествено число.
(define (numberOfDividors n)
  (define (helper number i)
    (if (>= i number) 0
        (if (= (remainder number i) 0) (+ 1 (helper number (+ i 1)))
            (helper number (+ i 1)))))
    (helper n 2))

;Дали число е четно.
(define (evenNumber? n)
  (= (remainder n 2) 0))

;Пресмята х на степен у.
(define (exponent x y)
  (define (helperExponent n numberInPower)
    (if (= numberInPower 0) 1
        (* n (helperExponent n (- numberInPower 1)))))
  (helperExponent x y))