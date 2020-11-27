module Cis194.Hw.Golf where

import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

skips :: [a] -> [[a]]
skips w =
  let ts = zip [1..] w
  in map (everyNth ts) $ map fst ts

everyNth :: [(Integer, a)] -> Integer -> [a]
everyNth ts n = map snd $ filter (\(i, _) -> i `mod` n == 0) ts

localMaxima :: [Integer] -> [Integer]
localMaxima =
  map (\[_, m, _] -> m)
  . filter (\[a, b, c] -> a < b && b > c)
  . filter (\l -> length l == 3)
  . map (take 3)
  . tails

histogram :: [Integer] -> String
histogram list = plot list ++ replicate 10 '=' ++ "\n0123456789\n"

counts :: [Integer] -> Map Integer Integer
counts = foldl inc $ Map.fromList $ zip [0..9] $ replicate 10 0
  where inc m i = Map.adjust (+1) i m

plot :: [Integer] -> String
plot list = unlines $ map row range
  where m = counts list
        mx = maximum $ Map.elems m
        range = [mx, (mx - 1)..1]
        char r c = if (m ! c) >= r then '*' else ' '
        row r = map (char r) [0..9]
