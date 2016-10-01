{-# LANGUAGE TemplateHaskell #-}

module HDNN.Type.Shape where

import Data.Singletons.TH
import Data.Singletons.Prelude

import HDNN.Prelude


$(singletons [d|
  data N = Z | S N
    deriving (Eq, Ord)

  |])

class Concats (d:: N) (xs::[Nat]) (ys::[Nat]) where
  type ConcatShape d xs ys :: [Nat]

instance (xs ~ ys) => Concats Z (x:xs) (y:ys) where
  type ConcatShape Z (x:xs) (y:_) = (x + y : xs)

instance (x ~ y) => Concats (S n) (x:xs) (y:ys) where
  type ConcatShape (S n) (x:xs) (_:ys) = x : ConcatShape n xs ys
