#lang racket
;Задача 1. Да се дефинира функция (prod-sum-div lst k), която намира произведението на естествените числа в списъка lst,
;сумата от делителите на които е кратна на k.
;Пример: (prod-sum-div '(1 3 6 8 9 10) 4) → 18 (3*6)
(define (sumOfDivisors n)
  (define (helper count number)
    (if (> count number) 0
        (if(= (remainder number count) 0) (+ count (helper (+ count 1) number))
           (helper (+ count 1) number))))
  (helper 1 n))

(define (devidesWithoutRes? firstNumber secondNumber)
  (= (remainder firstNumber secondNumber) 0))

(define (prod-sum-div l k)
  (if (null? l) 1
      (if (devidesWithoutRes? (sumOfDivisors (car l)) k) (* (car l) (prod-sum-div (cdr l) k))
          (prod-sum-div (cdr l) k))))

;Задача 2. Нека l1 = (a1 a2 … ak) и l2 = (b1 b2 … bk) са непразни списъци с еднакъв брой числа. Да се дефинира предикат
;(image? l1 l2), който да връща „истина“ точно когато съществува такова число x, че ai = x + bi за всяко i = 1,..., k.
;Пример 1: (image? ‘(1 2 3) ‘(4 5 6)) → #t
;Пример 2: (image? ‘(1 2 3) ‘(1 2 2)) → #f
(define (image? l1 l2)
  (if (null? l1) #f
      (if (and (= (remainder (car l2) (car l1)) 0) (not (= (car l1) (car l2)))) #t
          (image? (cdr l1) (cdr l2)))))

;Задача 3. Да се дефинира предикат (triangular? mat), който получава квадратна числова матрица, представена като списък от
;списъци, и проверява дали тя е горно триъгълна, т.е. дали всичките елементи под главния ѝ диагонал са нули.
;Пример 1: (triangular? '((1 2 3) (0 5 6) (0 0 9))) → #t
;Пример 2: (triangular? '((0 2 3) (0 0 6) (1 0 0))) → #f
(define (firstN? l n elem)
  (if (= n 0) #t
      (if(not (= (car l) elem)) #f
         (firstN? (cdr l) (- n 1) elem))))

(define (helperTriangle l count)
  (if (null? l) #t
      (if (not (firstN? (car l) count 0)) #f
          (helperTriangle (cdr l) (+ count 1)))))

(define (triangle? l)
  (if (null? l) #t
      (helperTriangle l 0)))
      