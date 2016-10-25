
module HDNN.Type.Shape where

import Data.Singletons.TH
import Data.Singletons.Prelude

import HDNN.Prelude

import Type.Family.Nat as Export (N(..))

dim0 = snat @ 0
dim1 = snat @ 1
dim3 = snat @ 3
dim4 = snat @ 4
dim5 = snat @ 5


type family Unary (d::Nat) :: N where
  Unary 0 = Z
  Unary n = S (Unary (n - 1))

class CanConcat d (xs::[Nat]) (ys::[Nat]) where
  type Concats d xs ys :: [Nat]

instance CanConcat (n::Nat) xs ys where
  type Concats n xs ys = Concats (Unary n) xs ys

instance CanConcat Z '[] ys where
  type Concats Z '[] _ = TypeError (Text "concat index out of range")

instance CanConcat Z xs '[] where
  type Concats Z _ '[] = TypeError (Text "concat index out of range")

instance (xs ~ ys) => CanConcat Z (x:xs) (y:ys) where
  type Concats Z (x:xs) (y:_) = (x + y : xs)

instance (x ~ y) => CanConcat (S n) (x:xs) (y:ys) where
  type Concats (S n) (x:xs) (_:ys) = x : Concats n xs ys





-- type family Concats2 (d::Nat) (xs::[Nat]) (ys::[Nat]) where
--   ConcatShape2 0 xs ys = (x:xs) (y:_) = (x + y : xs)
--   ConcatShape2 n xs ys = (x:xs) (y:ys) = x : ConcatShape n xs ys
