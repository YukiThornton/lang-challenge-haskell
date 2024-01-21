module Golf where

skipNth :: [a] -> Int -> [a]
skipNth list n = case drop n list of
    [] -> []
    (x:xs) -> x : skipNth xs n

skips :: [a] -> [[a]]
skips [] = []
skips list = zipWith skipNth (replicate (length list) list) [0..]

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:y:z:xs)
 | (x < y) && (y > z) = y:localMaxima (y:z:xs)
localMaxima list = localMaxima (drop 1 list)

occurence :: Eq t => t -> [t] -> Int
occurence target list = length (filter (target==) list)

star :: Int -> String
star num
 | num > 0 = "*"
 | otherwise = " "

toLines :: [Int] -> [String]
toLines list = case maximum list of
    0 -> []
    _ -> concatMap star list : toLines (map (\num -> num -1) list)

histogram :: [Integer] -> String
histogram list = unlines (reverse (toLines (map (`occurence` list) [0..9]))++[replicate 10 '=',concatMap show [0..9]])
