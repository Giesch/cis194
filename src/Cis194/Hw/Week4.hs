module Cis194.Hw.Week4 where

import qualified Data.Set as Set

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate fn
  where fn i = if even i then div i 2 else 3 * i + 1

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert item Leaf = Node 0 Leaf item Leaf
insert item (Node _ l i r) =
  let (l', r') = if height l < height r then (insert item l, r) else (l, insert item r)
  in Node (1 + max (height l') (height r')) l' i r'

height :: Tree a -> Integer
height (Node h _ _ _) = h
height Leaf = -1

xor :: [Bool] -> Bool
xor = foldr f False
  where f False v = v
        f True v = not v

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\item acc -> f item : acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n
  | n < 2 = []
  | otherwise = (2:) $ map (\i -> 2 * i + 1) $ filter (sieve n) [1..n]

sieve :: Integer -> Integer -> Bool
sieve n item =
  Set.notMember item $ Set.fromList $ takeWhile (<= n)
  [i + j + 2 * i * j | i <- [1..n], j <- [i..n]]
