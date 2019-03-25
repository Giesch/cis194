{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cis194.Hw.Scrabble where

import Data.Char
import Data.List

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score ch
  | oneOf "aeilnorstu" = 1
  | oneOf "dg" = 2
  | oneOf "bcmp" = 3
  | oneOf "fhvwy" = 4
  | oneOf "k" = 5
  | oneOf "jx" = 8
  | oneOf "qz" = 10
  | otherwise = 0
  where oneOf s = toLower ch `elem` s

scoreString :: String -> Score
scoreString = foldl' (\s ch -> s + score ch) (Score 0)
