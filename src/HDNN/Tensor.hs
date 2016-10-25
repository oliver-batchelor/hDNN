{-# LANGUAGE PartialTypeSignatures, NoMonomorphismRestriction #-}

module HDNN.Tensor where

--import GHC.TypeLits
import Data.Kind (Type)

import GHC.TypeLits
import GHC.TypeLits.List

import Data.Singletons.Prelude

import Data.Proxy

import HDNN.Type.Shape
import HDNN.Prelude


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
--
-- concat :: SNat dim -> Tensor ds a -> Tensor ds' a -> Tensor (ConcatShape dim ds ds') a
-- concat _ Tensor Tensor = Tensor




concat ::  SNat dim -> Tensor xs a -> Tensor ys a -> Tensor (Concats dim xs ys) a
concat _ Tensor Tensor = Tensor

concatMat ::  SNat dim -> Matrix m n a -> Matrix p q a -> Tensor (Concats dim [m, n] [p, q]) a
concatMat = concat

concatVec ::  Vector n a -> Vector m a -> Vector (n + m) a
concatVec = concat (snat @ 0)

-- baz :: _
-- baz m1 m2 = concat dim1 m1 m2
--
-- -- baz2 :: Matrix m m Float -> _ -> _
-- -- baz2 m1 m2 = concatMat dim2 m1 m2
--
-- baz3 :: Matrix 4 4 Float -> _
-- baz3 m1 m2 = concatMat dim0 m2 x where
--   (x::_) = baz m1 m1

-- baz4 :: Matrix 4 6 Float -> Matrix 4 4 Float -> Matrix _ _ Float
-- baz4 m1 m2 = baz3 m1 m2

-- concat0 :: (ds ~ ds') => Tensor (d0 ': ds) a -> Tensor (d0' : ds') a -> Tensor (d0 + d0' : ds) a
-- concat0 Tensor Tensor = Tensor
--
-- concat1 :: (d0 ~ d0', ds ~ ds') => Tensor (d0 : d1 : ds) a -> Tensor (d0' : d1' : ds') a -> Tensor (d0 : d1 + d1' : ds) a
-- concat1 Tensor Tensor = Tensor
--
-- concat2 :: (d0 ~ d0', d1 ~ d1', ds ~ ds') => Tensor (d0 : d1 : d2 : ds) a -> Tensor (d0' : d1' : d2' : ds') a -> Tensor (d0 : d1 : d2 + d2' : ds) a
-- concat2 Tensor Tensor = Tensor
