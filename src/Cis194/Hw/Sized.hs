{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Cis194.Hw.Sized where

import Data.Monoid ()

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

-- why is the name the opposite of the signature
getSize :: Size -> Int
getSize (Size i) = i


class Sized a where
  size :: a -> Size
  intSize :: a -> Int
  intSize s = getSize (size s)

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd

instance Monoid Size where
  mempty  = Size 0
  mappend = (+)
