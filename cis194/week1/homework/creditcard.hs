toDigits :: Integer -> [Integer]
toDigits n
 | n <= 0 = []
 | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
 | n <= 0 = []
 | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = [x, y * 2] ++ doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
 | x < 10 = x + sumDigits xs
 | otherwise = x `div` 10 + x `mod` 10 + sumDigits xs

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0