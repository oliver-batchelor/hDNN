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

module Typed (Ty(..),Typed,ty, module Data.IsTy) where

import Control.Applicative (liftA2)

-- ty package

import Data.Type.Equality
import Data.IsTy

import GHC.TypeLits
import GHC.TypeLits.List
import Tensor

-- | Typed type representation.  Alternative to Data.Ty in the ty package
data Ty a where
  Bool    :: Ty Bool
  Int     :: Ty Int
  Float   :: Ty Float

  Tensor :: NatList dims -> Ty a -> Ty (Tensor dims a)
  (:*:)   :: Ty a -> Ty b -> Ty (a, b)
  (:->:)  :: Ty a -> Ty b -> Ty (a -> b)

liftEq2 :: (a :~: a') -> (b :~: b') -> (f a b :~: f a' b')
liftEq2 Refl Refl = Refl


instance IsTy Ty where
  Bool     `tyEq` Bool       = Just Refl
  Int      `tyEq` Int        = Just Refl
  Float    `tyEq` Float      = Just Refl
  (a:*:b)  `tyEq` (a':*:b')  = liftA2 liftEq2 (tyEq a a') (tyEq b b')
  (a:->:b) `tyEq` (a':->:b') = liftA2 liftEq2 (tyEq a a') (tyEq b b')

  (Tensor ns a) `tyEq` (Tensor ns' a') = do
    Refl <- a `tyEq` a'
    Refl <- sameNats ns ns'
    return Refl

  tyEq _     _               = Nothing


-- | Replacement for Typeable and the ty package's ty.
class Typed a where ty :: Ty a

instance Typed Bool                               where ty = Bool
instance Typed Int                                where ty = Int
instance Typed Float                              where ty = Float
instance (Typed a, KnownNats dims) => Typed (Tensor dims a) where
  ty = Tensor natsList ty

instance (Typed a, Typed b) => Typed (a,b)  where ty = ty :*: ty
instance (Typed a, Typed b) => Typed (a->b) where ty = ty :->: ty
