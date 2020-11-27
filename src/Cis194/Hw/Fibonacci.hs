{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Cis194.Hw.Fibonacci where

----------
-- Ex 1 --
----------

-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function definition of type:
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

--
-- so that fib n computes the nth Fibonacci number Fn. Then, use fib to
-- define the infinite list of all Fibonacci numbers:
--
-- fibs1 :: [Integer]

fibs1 :: [Integer]
fibs1 = map fib [0..]

----------
-- Ex 2 --
----------

-- Define the infinite list:
--
-- fibs2 :: [Integer]
--
-- so that it has the same elements as fibs, but computing the first n
-- elements of fibs2 requires only O(n) addition operations. Be sure to
-- use standard recursion pattern(s) from Prelude, as appropriate.
--

fibs2 :: [Integer]
fibs2 = map (\(_, _, n) -> n)
  $ iterate (\(_, i, j) -> (i, j, i + j))
  (-1, 1, 0)

----------
-- Ex 3 --
----------

-- * Define a data type of polymorphic streams, Stream.
-- * Write a function to convert a Stream to an infinite list:
--
--   streamToList :: Stream a -> [a]

data Stream a = Cons a (Stream a)

streamToList :: Stream a  -> [a]
streamToList (Cons i s) = i : streamToList s

--
-- * Make your own instance of Show for Stream:
--
--   instance Show a => Show (Stream a) where
--     show ...
--
--   ...which works by showing only some prefix of a stream (say, the first
--   20 elements)

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

----------
-- Ex 4 --
----------

-- * Write a function:
--
-- streamRepeat :: a -> Stream a
--
-- ...which generates a stream containing infinitely many copies of the
-- given element
--

streamRepeat :: a -> Stream a
streamRepeat i = Cons i $ streamRepeat i

-- * Write a function:
--
-- streamMap :: (a -> b) -> Stream a -> Stream b
--
-- ...which applies a function to every element of a Stream
--

instance Functor Stream where
  fmap f (Cons i s) = Cons (f i) $ fmap f s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

-- * Write a function:
--
-- streamFromSeed :: (a -> a) -> a -> Stream a
--
-- ...which generates a Stream from a “seed” of type a, which is the first
-- element of the stream, and an “unfolding rule” of type a -> a which
-- specifies how to transform the seed into a new seed, to be used for
-- generating the rest of the stream.

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed g i = Cons i $ streamFromSeed g $ g i

----------
-- Ex 5 --
----------

-- * Define the stream:
--
-- nats :: Stream Integer
--
-- ...which contains the infinite list of natural numbers 0, 1, 2...
--

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- * Define the stream:
--
-- ruler :: Stream Integer
--
-- ...which corresponds to the ruler function:
--
-- 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
--
-- ...where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly divides n.
--
-- Hint: define a function interleaveStreams which alternates the
-- elements from two streams. Can you use this function to implement ruler
-- in a clever way that does not have to do any divisibility testing?

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons i s) t = Cons i (interleaveStreams t s)

ruler :: Stream Integer
ruler = rulerN 0
  where rulerN n = interleaveStreams (streamRepeat n) (rulerN (n + 1))

-- Exercise 6

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = fmap negate
  (+) (Cons i s) (Cons j t) = Cons (i + j) $ s + t
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) $ fromInteger a0 * b' + a' * b

instance Fractional (Stream Integer) where
  (/) a@(Cons a0 a') b@(Cons b0 b') =
    Cons (div a0 b0)
    $ (a' - (a / b) * b') / (fromInteger b0)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer

dot :: (Integer, Integer) -> (Integer, Integer) -> Integer
dot (a, b) (c, d) = a * c + b * d

instance Num Matrix where
  (*) (Matrix a b c d) (Matrix e f g h) =
    Matrix (dot (a, b) (e, g)) (dot (a, b) (f, h))
           (dot (c, d) (e, g)) (dot (c, d) (f, h))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n =
  let (Matrix _ f _ _) = Matrix 1 1 1 0 ^ n
  in f

fibs4 :: [Integer]
fibs4 = map fib4 [0..]
