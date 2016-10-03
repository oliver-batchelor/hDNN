{-# LANGUAGE PolyKinds #-}

----------------------------------------------------------------------
-- |
-- Module      :  CustomTy
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Custom Ty & Typable
----------------------------------------------------------------------

module HDNN.Typed (Ty(..), PrimTy(..), Prim(..), Typed, ty) where

import Control.Applicative (liftA2)

-- ty package

import Data.Type.Equality
import Data.IsTy

import HDNN.Prelude
import HDNN.Tensor

data PrimTy a where
  Bool    :: PrimTy Bool
  Int     :: PrimTy Int
  Float   :: PrimTy Float

-- | Typed type representation.  Alternative to Data.Ty in the ty package
data Ty a where
  T :: NatList dims -> PrimTy a -> Ty (Tensor dims a)
  Pair   :: Ty a -> Ty b -> Ty (a, b)

liftEq2 :: (a :~: a') -> (b :~: b') -> (f a b :~: f a' b')
liftEq2 Refl Refl = Refl

instance TestEquality PrimTy where
  Bool     `testEquality` Bool       = Just Refl
  Int      `testEquality` Int        = Just Refl
  Float    `testEquality` Float      = Just Refl


instance TestEquality Ty where
  Pair a b `testEquality` (Pair a' b')  = liftEq2 <$> testEquality a a' <*> testEquality b b'
  (T ns a)    `testEquality` (T ns' a') = liftEq2 <$> sameNats ns ns' <*> testEquality a a'

  _ `testEquality`    _               = Nothing

--
class Prim a where primTy :: PrimTy a

instance Prim Bool                               where primTy = Bool
instance Prim Int                                where primTy = Int
instance Prim Float                              where primTy = Float

-- | Replacement for Typeable and the ty package's ty.
class Typed a where ty :: Ty a

instance (Prim a, KnownNats dims) => Typed (Tensor dims a) where
  ty = T natsList primTy

instance (Typed a, Typed b) => Typed (a,b)  where
  ty = Pair ty ty
