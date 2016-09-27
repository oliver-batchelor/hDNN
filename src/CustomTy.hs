{-# LANGUAGE GADTs, ScopedTypeVariables, KindSignatures, DataKinds, TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
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

module CustomTy (Ty(..),Typeable,ty, module Data.IsTy) where

import Control.Applicative (liftA2)

import Numeric.LinearAlgebra.Static
import GHC.TypeLits
import Data.Proxy

import Unsafe.Coerce

-- ty package
import Data.Proof.EQ
import Data.IsTy

-- | Typed type representation.  Alternative to Data.Ty in the ty package
data Ty a where
  Bool    :: Ty Bool
  Integer :: Ty Integer
  Float   :: Ty Float
  Vector  :: Integer -> Ty (R n)
  (:*:)   :: Ty a -> Ty b -> Ty (a,b)
  (:->:)  :: Ty a -> Ty b -> Ty (a -> b)

instance IsTy Ty where
  Bool     `tyEq` Bool       = Just Refl
  Integer  `tyEq` Integer    = Just Refl
  Float    `tyEq` Float      = Just Refl
  Vector n `tyEq` Vector n'  
    | n == n'   = unsafeCoerce Refl
    | otherwise = Nothing
                                
  
  (a:*:b)  `tyEq` (a':*:b')  = liftA2 liftEq2 (tyEq a a') (tyEq b b')
  (a:->:b) `tyEq` (a':->:b') = liftA2 liftEq2 (tyEq a a') (tyEq b b')
  tyEq _     _               = Nothing


-- | Replacement for Typeable and the ty package's ty.
class Typeable a where ty :: Ty a

instance Typeable Bool                               where ty = Bool
instance Typeable Integer                            where ty = Integer
instance Typeable Float                              where ty = Float
instance KnownNat (n :: Nat) => Typeable (R n)       where ty = Vector (natVal (Proxy @ n))
      
      
instance (Typeable a, Typeable b) => Typeable (a,b)  where ty = ty :*: ty
instance (Typeable a, Typeable b) => Typeable (a->b) where ty = ty :->: ty
