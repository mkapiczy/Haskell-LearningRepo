module Card where

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
reverseList :: [Integer] -> [Integer]

toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

reverseList []     = []
reverseList (x:xs) = reverseList xs ++ [x]

toDigits n
  | n <=0 = []
  | otherwise = reverseList (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther [x]        = [x]
doubleEveryOther (x:(y:zs)) = x : (y*2) : doubleEveryOther zs

sumOfListElements :: [Integer] -> Integer
sumOfListElements []     = 0
sumOfListElements [x]    = x
sumOfListElements (x:xs) = x + sumOfListElements xs

sumOfDigits :: Integer -> Integer
sumOfDigits n
  | n < 10 = n
  | otherwise = sumOfListElements(toDigits n)

listOfNumbersToDigitsList :: [Integer] -> [Integer]
listOfNumbersToDigitsList [] = []
listOfNumbersToDigitsList (x:xs) = (toDigits x) ++ listOfNumbersToDigitsList xs

cardCheckSum :: Integer -> Integer
cardCheckSum n = sumOfListElements(listOfNumbersToDigitsList(doubleEveryOther(toDigitsRev n)))

validate :: Integer -> Bool
validate n = ((cardCheckSum n) `mod` 10) == 0
