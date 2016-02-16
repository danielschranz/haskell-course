{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calc where
import ExprT
import Parser
import Control.Monad
import qualified StackVM
import qualified Data.Map as M
import Data.Maybe

-- ex 1
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Mul a b) = eval a * eval b
eval (ExprT.Add a b) = eval a + eval b

-- ex 2
-- evalStr :: String -> Maybe Integer
-- evalStr x = doEval (parseExp ExprT.Lit ExprT.Add ExprT.Mul x)
--   where doEval :: Maybe ExprT -> Maybe Integer
--         doEval Nothing = Nothing
--         doEval (Just y) = Just (eval y)

-- a lot nicer (thanks Mark to find Control.Monad ap in the first place)
evalStr :: String -> Maybe Integer
evalStr x = liftM eval (parseExp ExprT.Lit ExprT.Add ExprT.Mul x)



-- ex 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit a = ExprT.Lit a
  add a b = ExprT.Add a b
  mul a b = ExprT.Mul a b

-- ex 4

instance Expr Integer where
  lit = id
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit = (<) 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit a = MinMax a
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit a = Mod7 $ mod a 7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a+b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a*b) 7

-- ex 5

instance Expr StackVM.Program where
  lit a = StackVM.PushI a : []
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile x = (parseExp lit add mul x) :: Maybe StackVM.Program

-- compile "(2+3)*4"   =>   Just [PushI 2,PushI 3,Add,PushI 4,Mul]

--ex 6

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

class HasVars a where
  var :: String -> a

instance HasVars VarExprT where
  var x = Var x

instance Expr VarExprT where
  lit x = Calc.Lit x
  add a b = Calc.Add a b
  mul a b = Calc.Mul a b


-- now it gets interesting we want to look up stuff as well
instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = M.lookup x

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = (\ignoredMap -> Just x)  -- return a pretend map lookup that returns a constant value

  add a b = (\m -> if isJust (a m) && isJust (b m) then Just (fromJust(a m) + fromJust (b m)) else Nothing)  -- pass map along in case it's needed in subexpression for an actual lookup (with var from the type class above
  mul a b = (\m -> if isJust (a m) && isJust (b m) then Just (fromJust(a m) * fromJust (b m)) else Nothing)


-- test with:
--withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
--withVars vs exp = exp $ M.fromList vs
--tests:
-- *Calc> :t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
-- *Calc> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- *Expr> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- *Calc> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54
