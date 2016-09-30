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

module HDNN.Typed (Ty(..), PrimTy(..), IsPrim(..), Typed,ty, module Data.IsTy) where

import Control.Applicative (liftA2)

-- ty package

import Data.Type.Equality
import Data.IsTy

import GHC.TypeLits
import GHC.TypeLits.List
import HDNN.Tensor

data PrimTy a where
  Bool    :: PrimTy Bool
  Int     :: PrimTy Int
  Float   :: PrimTy Float

-- | Typed type representation.  Alternative to Data.Ty in the ty package
data Ty a where
  T :: NatList dims -> PrimTy a -> Ty (Tensor dims a)
  Pair   :: Ty a -> Ty b -> Ty (a, b)
  (:->)  :: Ty a -> Ty b -> Ty (a -> b)

liftEq2 :: (a :~: a') -> (b :~: b') -> (f a b :~: f a' b')
liftEq2 Refl Refl = Refl

instance IsTy PrimTy where
  Bool     `tyEq` Bool       = Just Refl
  Int      `tyEq` Int        = Just Refl
  Float    `tyEq` Float      = Just Refl


instance IsTy Ty where
  Pair a b `tyEq` (Pair a' b')  = liftEq2 <$> tyEq a a' <*> tyEq b b'
  (a :-> b)  `tyEq` (a' :-> b') = liftEq2 <$> tyEq a a' <*> tyEq b b'
  (T ns a)    `tyEq` (T ns' a') = liftEq2 <$> sameNats ns ns' <*> tyEq a a'

  tyEq _     _               = Nothing

--
class IsPrim a where primTy :: PrimTy a

instance IsPrim Bool                               where primTy = Bool
instance IsPrim Int                                where primTy = Int
instance IsPrim Float                              where primTy = Float

-- | Replacement for Typeable and the ty package's ty.
class Typed a where ty :: Ty a

instance (IsPrim a, KnownNats dims) => Typed (Tensor dims a) where
  ty = T natsList primTy

instance (Typed a, Typed b) => Typed (a,b)  where ty = Pair ty ty
instance (Typed a, Typed b) => Typed (a->b) where ty = ty :-> ty
