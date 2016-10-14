{-# LANGUAGE TypeFamilies, DataKinds, TypeInType, GADTs, KindSignatures, InstanceSigs, RankNTypes, ScopedTypeVariables, TypeOperators #-}

module Exp where

import Control.Monad.State
import Data.Maybe

import GHC.TypeLits
--import GHC.TypeLits.List
import Data.Kind (Type)
import Data.Proxy

import Type.Family.Monoid

-- * Simply-typed Lambda Calculus

data Tensor (ds :: [Nat]) a = Tensor

type Matrix n m = Tensor '[n, m]
type Vector n = Tensor '[n]


data Op a where
  Linear :: Op (Matrix n m Float -> Vector n Float -> Vector m Float)
  --Flatten :: KnownNats ns => Tensor ns a -> Vector (Product ns) a

data SNat (n :: Nat) where
  SNat :: KnownNat n => Proxy n -> SNat n


data Exp m a where
    Op :: Op a -> Exp e hs a
    Input :: IntMap e m => Proxy e -> SNat n -> Exp p m (Vector n a)
    App  ::  Exp k h (a -> b) -> Exp k h' a -> Exp k (m <> m') b



class IntMap k m where
  type Inj k (n :: Nat) m :: m


instance IntMap () Nat where
  type Inj () n m = n + m


--data Input =
