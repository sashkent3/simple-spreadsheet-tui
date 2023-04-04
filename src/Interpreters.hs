{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Interpreters where

import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError, modifyError, throwError)
import Control.Monad.Trans (lift)
import Data.Functor ((<&>))
import Data.Functor.Dialects (Add ((:+:), (:-:)), Div ((:/:)), Mul ((:*:)), Sign (Neg, Pos))
import Data.Functor.Sum (Sum (InL, InR))
import Data.Spreadsheet (EvalError)

infixr 5 :|:, ?:

type f :|: g = Sum f g

(?:) :: (f a -> c) -> (g a -> c) -> (f :|: g) a -> c
(?:) f g = \case InL x -> f x; InR x -> g x

-- | A type of errors which can happen during @Div@ evaluation.
data DivError = DivByZero | DivEval EvalError

instance Show DivError where
  show :: DivError -> String
  show DivByZero = "#DIV/0!"
  show (DivEval e) = show e

runSign :: (Num a, Applicative f) => Sign (f a) -> f a
runSign (Pos x) = x
runSign (Neg x) = negate <$> x

runAdd :: (Num a, Applicative f) => Add (f a) -> f a
runAdd = \case
  x :+: y -> liftA2 (+) x y
  x :-: y -> liftA2 (-) x y

runMul :: (Num a, Applicative f) => Mul (f a) -> f a
runMul (x :*: y) = liftA2 (*) x y

runDiv :: (Fractional a, Eq a, MonadError DivError m) => Div (m a) -> m a
runDiv (x :/: y) =
  y >>= \case
    0 -> throwError DivByZero
    y' -> x <&> (/ y')

type NumResult = Either EvalError

type NumDialect = (Sign :|: Add :|: Mul)

runNum :: Num a => NumDialect (NumResult a) -> NumResult a
runNum = runSign ?: runAdd ?: runMul

type AllResult = ExceptT EvalError (Either DivError)

type AllDialect = (Sign :|: Add :|: Mul :|: Div)

runAll ::
  (Fractional a, Eq a) => AllDialect (AllResult a) -> AllResult a
runAll = runSign ?: runAdd ?: runMul ?: lift . runDiv . fmap (modifyError DivEval)

showAllResult :: Show a => AllResult a -> String
showAllResult = either show show . modifyError DivEval