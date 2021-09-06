#lang racket
;Искаме да намерим сумата от цифрите на дадено число. Ама итеративно.
(define (sumOfDigits n)
  (if (< n 10) n
      (+ (remainder n 10) (sumOfDigits (quotient n 10)))))

;Искаме да обърнем число от двоична в десетична бройна система.
(define (helper degree number)
   (if (= number 0) 0
       (+ (* (remainder number 10) (expt 2 degree)) (helper (+ degree 1) (quotient number 10)))))
(define (toDecimal n)
  (if (= n 0) 0
      (helper 0 n)))  

;Извличането на n-тия пореден елемент (n>0) на даден списък l.
(define (nThElem n l)
  (if (= n 1) (car l)
      (nThElem (- n 1) (cdr l))))

;Намиране на броя на елементите (дължината) на даден списък.
(define (lenList l)
  (if (null? l) 0
      (+ 1 (lenList (cdr l)))))

;Обединяване на елементите на произволен брой списъци.
(define (appendLists l1 l2)
  (if (null? l1) l2
      (cons (car l1) (appendLists (cdr l1) l2))))

;Да се конструира списъкът с числата от start до end.
(define (constructL start end)
  (if (> start end) '()
      (cons start (constructL (+ start 1) end))))

;Обръща елементите на списък.
(define (myReverse l)
  (define (helperReverse l1 reversed)
    (if (null? l1) reversed
        (helperReverse (cdr l1) (cons (car l1) reversed))))
  (helperReverse l `()))

;Проверка дали даден елемент принадлежи на списък.
(define (myMember? l elem)
  (if (null? l) #f
      (if (= elem (car l)) #t
          (myMember? (cdr l) elem))))

;Намира минималния елемент в списък.
(define (minElem l)
  (define (helperMin l1 tempMin)
    (if (null? l1) tempMin
        (if (< (car l1) tempMin) (helperMin (cdr l1) (car l1))
            (helperMin (cdr l1) tempMin)))) 
  (helperMin l (car l)))

;Задача 2.Да се дефинира функцията (count-minimum xs), която връща броя на срещанията на най-малкия елемент на списъка xsв него.
(define (count-minimum l)
  (if (null? l) 0
      (if (= (car l) (minElem l)) (+ 1 (count-minimum (cdr l)))
          (count-minimum (cdr l)))))

;Задача 3.Да се дефинира функцията (sieve-of-eratosthenes n), която приема целочисления аргумент n и връща списък с всички прости
;числа по-малки или равни на n.
(define (helperPrime? number count)
  (if (= count number) #t
      (if (= (remainder number count) 0) #f
          (helperPrime? number (+ count 1)))))

(define (isPrime? n)
  (if (= n 1) #f
      (helperPrime? n 2)))

(define (helper2 count n list)
  (if (> count n) list
      (if (isPrime? count) (helper2 (+ count 1) n (cons count list))
          (helper2 (+ count 1) n list))))

(define (listFromPrimeSmallerThanN n)
  (if (<= n 1) `()
        (helper2 2 n `())))

;Сечение на двата списъка (приемаме, че няма повтарящи се елементи).
(define (intersectionOfLists l1 l2)
  (define (helper fL sL result)
    (if (null? fL) result
        (if (myMember? sL (car fL)) (helper (cdr fL) sL (cons (car fL) result))
            (helper (cdr fL) sL result))))
  (helper l1 l2 `()))

;Искаме функция, която приема списък и две числа и връща списък, състоящ се от елементите на списъка, които се намират на индекси
;от първото число до второто.
(define (constuctListBetweenIndex lst fIndex sIndex)
  (define (helperF l1 first second count result)
    (if (< count first) (helperF (cdr l1) first second (+ count 1) result)
        (if (or (> count second) (null? l1) )result
            (helperF (cdr l1) first second (+ count 1) (cons (car l1) result)))))
  (myReverse (helperF lst fIndex sIndex 1 `())))
        