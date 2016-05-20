module Cis194.Hw.Golf where
import Data.List

g = length

skips :: [a] -> [[a]]
skips s = map (h s) [1..g s]

-- Every nth
h :: [a] -> Int -> [a]
h m n = [c | (c, i) <- zip m (cycle [1..n]), i == n]

localMaxima :: [Integer] -> [Integer]
localMaxima s = [b | (a, b, c) <- zip3 s (tail s) (drop 2 s), a < b && b > c]

histogram :: [Integer] -> String
histogram l = unlines (transpose [(replicate (b - g n) ' ') ++ n | n <- m])
    where m = [concat ["*" | x <-l, x == i] ++ "=" ++ show i| i <- [0..9]]
          b = maximum (map g m)
