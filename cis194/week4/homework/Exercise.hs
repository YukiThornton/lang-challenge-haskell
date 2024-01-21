module Exercise where
import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
 | even x = (x - 2) * fun1 xs
 | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\n -> n - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
 | even n = n + fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer
fun2' =  sum
        . filter even
        . takeWhile (/= 1)
        . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf
 | Node Integer (Tree a) a (Tree a)
 deriving (Show, Eq)

balanced :: Tree a -> Tree a -> Bool
balanced (Node levell _ _ _) (Node levelr _ _ _) = levell == levelr
balanced Leaf Leaf = True
balanced _ _ = False

height :: Tree a -> Integer
height Leaf = 0
height (Node level _ _ _) = level

addNode :: a -> Tree a -> Tree a
addNode target Leaf = Node 0 Leaf target Leaf
addNode target (Node _ Leaf el Leaf) = Node 1 (addNode target Leaf) el Leaf
addNode target (Node level left el Leaf) = Node level left el (addNode target Leaf)
addNode target (Node level Leaf el right) = Node level (addNode target Leaf) el right
addNode target (Node level left el right) = case compare (height left) (height right) of
    LT -> Node level (addNode target left) el right
    GT -> Node level left el (addNode target right)
    EQ -> Node (1 + height right') left el right'
        where right' = addNode target right

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf

xor :: [Bool] -> Bool
xor = foldr (\v acc -> if v then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\v acc -> acc++[f v]) []

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

removeList :: Integer -> [Integer]
removeList n = (filter (< n)
                . map (\(i, j) -> i + j + 2 * i * j)
                . filter (uncurry (<=)))
               (cartProd [1..n] [1..n])

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : (filter (< 2*n + 2)
                        . map (\n -> 2 * n + 1)) ([1..n] \\ removeList n)