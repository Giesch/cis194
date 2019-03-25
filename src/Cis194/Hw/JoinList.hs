{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JoinList where

import           Cis194.Hw.Buffer
import           Cis194.Hw.Scrabble
import           Cis194.Hw.Sized
import           Data.Monoid
import           Data.List

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                deriving (Eq, Show)

-- Exercise one

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

instance Monoid m => Monoid (JoinList m a) where
  mempty      = Empty
  mappend l r = Append (tag l <> tag r) l r

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = mappend

-- Exercise two

example :: JoinList (Product Int) Char
example =
  Append
    (Product 210)
    (Append
       (Product 30)
       (Single (Product 5) 'y')
       (Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a')))
    (Single (Product 7) 'h')

(!!?) :: [a] -> Int -> Maybe a
[]      !!? _         = Nothing
_       !!? i | i < 0 = Nothing
(x:_xs) !!? 0         = Just x
(_x:xs) !!? i         = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ a)   = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

-- this assumes that the m of the JoinList is a size cache
-- which seems like a bad design assumption tbh
instance (Sized m, Monoid m) => Sized (JoinList m a) where
  size jl = size $ tag jl

-- these are unnecessarily redundant wrt Empty & Single
-- I'm not sure how to clean it up w/o breaking exhaustiveness checking

indexJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a
indexJ n _ | n < 0            = Nothing
indexJ n jl | n >= intSize jl = Nothing
indexJ _ Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ n (Append _ l r)
  | n < intSize l   = indexJ n l
  | otherwise       = indexJ (n - intSize l) r

dropJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0  = jl
dropJ n jl | n >= intSize jl = Empty
dropJ _ Empty        = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l r)
  | n < intSize l = dropJ n l <> r
  | otherwise     = dropJ (n - intSize l) r

takeJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0           = Empty
takeJ n jl | n >= intSize jl = jl
takeJ _ Empty                = Empty
takeJ _ (Single _ _)         = Empty
takeJ n (Append _ l r)
  | n <= intSize l = takeJ n l
  | otherwise      = l <> takeJ (n - intSize l) r

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4

toSingle :: String -> JoinList (Score, Size) String
toSingle s = Single (scoreString s, Size (length s)) s

instance Buffer (JoinList (Score, Size) String) where
  toString jl = case jl of
    Empty          -> ""
    (Single _ s)   -> s
    (Append _ l r) -> toString l <> toString r

  -- TODO this is unbalanced/linear
  fromString s =
    foldl' (<>) Empty $ map toSingle $ lines s

  line = indexJ

  replaceLine n ln buf =
    takeJ n buf <> toSingle ln <> dropJ (n + 1) buf

  numLines jl = case jl of
    Empty          -> 0
    (Single _ _)   -> 1
    (Append _ l r) -> numLines l + numLines r

  value jl = let (Score sc, _) = tag jl in sc
