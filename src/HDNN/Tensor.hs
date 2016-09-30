module HDNN.Tensor where

--import GHC.TypeLits
import Data.Kind (Type)

import GHC.TypeLits
import GHC.TypeLits.List

import qualified Data.Vector.Unboxed as V

data Tensor (dims :: [Nat]) (a :: Type) = Tensor

type Vector (n :: Nat) a = Tensor '[n] a
type Matrix m n a = Tensor '[m, n] a
type Image h w c a = Tensor '[h, w, c] a

instance Show (Tensor ds a) where
  show _ = "Tensor"

instance (Show a, Num a, KnownNats ds) => Num (Tensor ds a) where
    fromInteger _ = Tensor
    (-) t1 t2 = Tensor
    (+) t1 t2 = Tensor
    (*) t1 t2 = Tensor
    abs t = Tensor
    signum t = Tensor


concat1 :: (ds ~ ds') => Tensor (d1 ': ds) a -> Tensor (d1' : ds') a -> Tensor (d1 + d1' : ds) a
concat1 Tensor Tensor = Tensor

concat2 :: (d1 ~ d1', ds ~ ds') => Tensor (d1 : d2 : ds) a -> Tensor (d1' : d2' : ds') a -> Tensor (d1 : d2 + d2' : ds) a
concat2 Tensor Tensor = Tensor

concat3 :: (d1 ~ d1', d2 ~ d2', ds ~ ds') => Tensor (d1 : d2 : d3 : ds) a -> Tensor (d1' : d2' : d3' : ds') a -> Tensor (d1 : d2 : d3 + d3' : ds) a
concat3 Tensor Tensor = Tensor
