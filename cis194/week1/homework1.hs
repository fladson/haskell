-- Validating Credit Card Numbers

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | n < 10 = [n]
  -- 123 = [1,2,3]
  | otherwise = [read [x] | x <- show n]

toDigitsReversed :: Integer -> [Integer]
toDigitsReversed n = reverse (toDigits n)

-- Exercise 2
-- Lost too much time trying to implement this in one expression =/
-- Need more experience with recursion
doubleEveryOtherEven :: [Integer] -> [Integer]
doubleEveryOtherEven [x, y] = [x * 2, y]
doubleEveryOtherEven (x : (y : zs)) = x * 2 : y : doubleEveryOtherEven zs

doubleEveryOtherOdd :: [Integer] -> [Integer]
doubleEveryOtherOdd [x] = [x]
doubleEveryOtherOdd [x, y] = [x, y * 2]
doubleEveryOtherOdd (x : (y : zs)) = x : y * 2 : doubleEveryOtherOdd zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x, y] = [x * 2, y]
doubleEveryOther n
  | odd (length n) = doubleEveryOtherOdd n
  | otherwise = doubleEveryOtherEven n

-- Exercise 3
-- Exercise 4

-- The Towers of Hanoi
-- Exercise 5
-- Exercise 6
