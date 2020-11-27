{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Cis194.Hw.Calc where 

import Cis194.Hw.ExprT
import Cis194.Hw.Parser (parseExp)
import qualified Cis194.Hw.StackVM as VM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit i = Mod7 (mod i 7)
  add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
  mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)

instance Expr VM.Program where
  lit i = [VM.PushI i]
  add a b = a ++ b ++ [VM.Add]
  mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (Map String Integer -> Maybe Integer) where
  var s = (\env -> Map.lookup s env)

instance Expr (Map String Integer -> Maybe Integer) where
  lit i = (\_ -> Just i)
  add f g = opFn (+) f g
  mul f g = opFn (*) f g

opFn :: (v -> v -> v)
     -> (Map k v -> Maybe v)
     -> (Map k v -> Maybe v)
     -> Map k v
     -> Maybe v
opFn o f g m = o <$> (f m) <*> (g m)

withVars :: [(String, Integer)]
         -> (Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs ex = ex $ Map.fromList vs
