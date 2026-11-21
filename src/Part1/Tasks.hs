module Part1.Tasks where

import Util(notImplementedYet)

factorial :: Integer -> Integer
factorial n = product [1..n]

-- сжимаем угол, чтобы он не вылезал за [-2pi; 2pi]
normalizeAngle :: Double -> Double
normalizeAngle x = x - 2 * pi * fromIntegral (truncate (x / (2 * pi)))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = mySinInternal (normalizeAngle x) 15

mySinInternal :: Double -> Integer -> Double
mySinInternal x 0 = x
mySinInternal x n = term + mySinInternal x (n - 1)
  where
    power = 2 * n + 1
    sign  = (-1) ^ n
    term  = sign * (x ^ power) / fromIntegral (factorial power)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = myCosInternal (normalizeAngle x) 15

myCosInternal :: Double -> Integer -> Double
myCosInternal x 0 = 1
myCosInternal x n = term + myCosInternal x (n - 1)
  where
    power = 2 * n
    sign  = (-1) ^ n
    term  = sign * (x ^ power) / fromIntegral (factorial power)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = myGCDInternal (abs a) (abs b)

myGCDInternal :: Integer -> Integer -> Integer
myGCDInternal a 0 = a
myGCDInternal 0 b = b
myGCDInternal a b = myGCDInternal maxModMin minNum
  where
    minNum    = min a b
    maxNum    = max a b
    maxModMin = mod maxNum minNum

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | year < 0 = False
  | month < 1 || month > 12 = False
  | day < 1 || day > 31 = False
  | not $ isDayOfMonthCorrect month day (isLeap year) = False
  | otherwise =  True

isLeap :: Integer -> Bool
isLeap year
  | mod year 400 == 0 = True
  | (mod year 4 == 0) && (mod year 100 /= 0) = True
  | otherwise = False

isDayOfMonthCorrect :: Integer -> Integer -> Bool -> Bool
isDayOfMonthCorrect month day isYearLeap =
  day >= 1 && day <= lastDayOfMonth 
  where
    lastDayOfMonth = case month of
      1  -> 31
      2  -> (if isYearLeap then 29 else 28)
      3  -> 31
      4  -> 30
      5  -> 31
      6  -> 30
      7  -> 31
      8  -> 31
      9  -> 30
      10 -> 31
      11 -> 30
      12 -> 31

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow n 0 = 1
myPow n k = n * (myPow n $ k - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = True
isPrime n = isPrimeInternal n 2

isPrimeInternal :: Integer -> Integer -> Bool
isPrimeInternal n k
  | n == k       = True
  | mod n k == 0 = False
  | otherwise    = isPrimeInternal n (k + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea [] = 0
shapeArea [_] = 0
shapeArea points@(h:_) =
  abs $ totalSum / 2
  where
    len          = toInteger $ length points
    cyclicPoints = points ++ [h]
    totalSum     = shapeAreaInternal cyclicPoints len

shapeAreaInternal :: [Point2D] -> Integer -> Double
shapeAreaInternal _ 0 = 0
shapeAreaInternal (a:b:rest) i =
  current + shapeAreaInternal (b:rest) (i - 1)
  where
    current = fst a * snd b - snd a * fst b

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | a <= 0 || b <= 0 || c <= 0      = -1
  | shortEdge + midEdge <= longEdge = -1
  | otherwise                       = case compare (square shortEdge + square midEdge) (square longEdge) of
    LT -> 0
    GT -> 1
    EQ -> 2
  where
    shortEdge = min a $ min b c
    longEdge = max a $ max b c
    midEdge = a + b + c - (shortEdge + longEdge)
    square x = x * x
