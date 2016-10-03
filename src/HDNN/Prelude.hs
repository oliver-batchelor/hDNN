{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-}

module HDNN.Prelude (module Export, module HDNN.Prelude) where

import  GHC.TypeLits as Export
import  GHC.TypeLits.List as Export
import  Data.Proxy as Export
import  Data.Kind  as Export (Type)


import Prelude as Export hiding (concat, id, (.))

data SNat (n :: Nat) = KnownNat n => SNat (Proxy n)

instance Show (SNat n) where
  show (SNat p) = 'd' : show (natVal p)

{-# INLINE snat #-}
-- | Create a singleton literal for a type-level natural number
snat :: KnownNat n => SNat n
snat = SNat Proxy

{-# INLINE withSNat #-}
-- | Supply a function with a singleton natural 'n' according to the context
withSNat :: KnownNat n => (SNat n -> a) -> a
withSNat f = f (SNat Proxy)

{-# INLINE snatToInteger #-}
snatToInteger :: SNat n -> Integer
snatToInteger (SNat p) = natVal p
