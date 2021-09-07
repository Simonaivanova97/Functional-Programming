--ghc 8.0.2
main = do
        print (mymin 5 3)
        print (mymax 3 10)
        print (myfunc 3 5)
        print (myfib 5)
        print(maxElement [2,3,6,1])
 
-- Да се напише функция mymin, която приема два аргумента и връща по-малкият от тях.
mymin :: Int -> Int -> Int 
mymin a b = if a<b
            then a 
            else b
            
-- Да се напише функция mymax, която приема два аргумента и връща по-големият от тях.
mymax :: Int -> Int -> Int
mymax a b = if a>b
            then a 
            else b
            
-- Да се напише функция myfunc, която пресмята на средно аритметичното от квадратите на 2 числа.
myfunc :: Double -> Double -> Double
myfunc a b = ((a*a)+(b*b))/2

-- Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи.
myfib 0 = 1
myfib 1 =1 
myfib n =  myfib(n-1)+myfib(n-2)

-- Дължина на списък.
len :: [Int] -> Int
len [] = 0
len (_:x) = 1+len x

-- Максимален елемент на списък.
maxElement::[Int]->Int
maxElement [x] = x
maxElement (y:xs) = if y>maxElement xs 
                        then y 
                        else maxElement xs

-- Обръща елементите на списък.
myReverse::[Int]->[Int]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- Проверява дали даден елемент е част от списък.
myMember :: [Int] -> Int -> Bool
myMember :: [] a = False
myMember (x:xs) a = x==a || myMember xs a
