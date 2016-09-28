module Tensor where

import GHC.TypeLits
import Data.Kind (Type)

data Tensor (dims :: [Nat]) (a :: Type) 

type Vector (n :: Nat) a = Tensor '[n] a
type Matrix m n a = Tensor '[m, n] a
type Image h w c a = Tensor '[h, w, c] a
