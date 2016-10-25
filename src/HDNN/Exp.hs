{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-}


-- Example of data-treify for a first pass of CSE on a simple typed
-- language representation.

module HDNN.Exp where


import Control.Applicative (pure,(<$>),(<*>))
import qualified Data.IntMap as I
import Data.Maybe (fromMaybe) -- ,fromJust

-- package ty
import Data.Type.Equality
import Control.Category
import Control.Arrow

-- Import one of the following but not both
--import Data.Ty
import HDNN.Typed
import HDNN.Tensor

import HDNN.Type.Shape
import HDNN.Prelude


-- data N :: (Type -> Type) -> Type -> Type where
--    Op  :: Op a -> N v a
--    App :: (Typed a, Typed b) =>
--           v (a -> b) -> v a -> N v b
--
-- instance ShowF v => ShowF (N v) where
--    showF (ON o)    = unwords ["ON" ,showF o]
--    showF (App a b) = unwords ["App",showF a,showF b]
