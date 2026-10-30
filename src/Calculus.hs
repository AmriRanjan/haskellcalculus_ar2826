module Calculus (sumTo, vars, inTermsOf, eval, pretty, diff, maclaurin, pi) where

import Prelude hiding (pi)

import Expr

import Data.Map (Map, (!))
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set

type Env = Map String Double

preOps :: Map PreOp (Double -> Double)
preOps = Map.fromList
  [ (Neg, negate)
  , (Sin, sin)
  , (Cos, cos)
  , (Log, log)
  , (Exp, exp)
  ]

binOps :: Map BinOp (Double -> Double -> Double)
binOps = Map.fromList
  [ (Add, (+))
  , (Mul, (*))
  , (Div, (/))
  ]

---------------------------------------------------------------------------
-- Type classes and class instances

instance Num Expr where
  fromInteger :: Integer -> Expr
  fromInteger = undefined
  negate      :: Expr -> Expr
  negate      = undefined
  (+), (*)    :: Expr -> Expr -> Expr
  (+)         = undefined
  (*)         = undefined

instance Fractional Expr where
  fromRational :: Rational -> Expr
  fromRational = undefined
  (/)          :: Expr -> Expr -> Expr
  (/)          = undefined

instance Floating Expr where
  sin, cos, log, exp :: Expr -> Expr
  sin = undefined
  cos = undefined
  log = undefined
  exp = undefined

---------------------------------------------------------------------------

{-|
Computes the sum of first `n` terms of the given series.
-}
sumTo :: Num a => Int -> [a] -> a
sumTo n xs = foldl (+) 0 (take n xs)

{-|
Computes the set of variable names that appear within the given expression.
-}
vars :: Expr -> Set String
vars (Val _) = Set.empty
vars (Id e) = Set.singleton e
vars (Pre _ e) = vars e
vars (Bin _ e1 e2) = Set.union (vars e1) (vars e2)

{-|
Does the given expression only use the given variable and no others?
-}
inTermsOf :: String -> Expr -> Bool
inTermsOf x e = Set.isSubsetOf (vars e) (Set.singleton x)

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Env -> Expr -> Double
eval env (Val n) = n
eval env (Id x) = env ! x
eval env (Pre op e) = (preOps ! op) (eval env e)
eval env (Bin op e1 e2) =
  (binOps ! op) (eval env e1) (eval env e2)

-- We could also use Map.! like below, but just having ! is more clean
-- eval :: Env -> Expr -> Double
-- eval env (Val n) = n
-- eval env (Id x) = env Map.! x
-- eval env (Pre op e) = (preOps Map.! op) (eval env e)
-- eval env (Bin op e1 e2) =
--   (binOps Map.! op) (eval env e1) (eval env e2)

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
pretty :: Expr -> String
pretty = show

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff = undefined

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin = undefined

-- Extension
instance Num a => Num [a] where
  fromInteger  :: Integer -> [a]
  fromInteger  = undefined
  negate       :: [a] -> [a]
  negate       = undefined
  (+), (*)     :: [a] -> [a] -> [a]
  (+)          = undefined
  (*)          = undefined

instance Fractional a => Fractional [a] where
  fromRational :: Rational -> [a]
  fromRational = undefined
  (/)          :: [a] -> [a] -> [a]
  (/)          = undefined

instance Floating a => Floating [a] where
  (**)         :: [a] -> [a] -> [a]
  (**)         = undefined

pi :: (Enum a, Fractional a) => Int -> a
pi = undefined
