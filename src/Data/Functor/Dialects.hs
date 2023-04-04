{-# LANGUAGE DeriveTraversable #-}

module Data.Functor.Dialects where

data Add a = a :+: a | a :-: a
  deriving (Read, Show, Functor, Foldable, Traversable)

data Mul a = a :*: a deriving (Read, Show, Functor, Foldable, Traversable)

data Div a = a :/: a deriving (Read, Show, Functor, Foldable, Traversable)

data Sign a = Pos a | Neg a deriving (Read, Show, Functor, Foldable, Traversable)
