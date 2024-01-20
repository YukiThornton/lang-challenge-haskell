sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial(n - 1)

hailstone :: Integer -> Integer
hailstone n
 | n `mod` 2 == 0 = n `div` 2
 | otherwise = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
 | "Haskell" > "C++" = 3
 | otherwise = 4
foo n
 | n < 0 = 0
 | n `mod` 17 == 2 = -43
 | otherwise = n + 3

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

hailStoneSeq :: Integer -> [Integer]
hailStoneSeq 1 = [1]
hailStoneSeq n = n : hailStoneSeq(hailstone n)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:(y:zs)) = (x + y): sumEveryTwo zs

hailStoneLen :: Integer -> Integer
hailStoneLen n = intListLength(hailStoneSeq n) - 1