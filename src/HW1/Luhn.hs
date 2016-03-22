module HW1.Luhn
    (
      toDigitsRev
    , toDigits
    , doubleEveryOther
    , sumDigits
    , validate
    ) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = mod n 10 : toDigitsRev (div n 10)
  | otherwise = []

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther s@(x:(y:xs))
  | length s `mod` 2 == 0 = x * 2 : y : doubleEveryOther xs
  | otherwise = x : y * 2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x >= 10 = x `mod` 10 + x `div` 10 + sumDigits xs
  | otherwise = x + sumDigits xs

validate :: Integer -> Bool
validate n = total `mod` 10 == 0
  where
    total = sumDigits . doubleEveryOther $ toDigits n
