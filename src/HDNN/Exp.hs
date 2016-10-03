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


data Node (ds :: [Nat]) a where
  Constant  :: PrimTy a -> Tensor sh a -> Node sh a
  Trainable :: PrimTy a -> Tensor sh a -> Node sh a

type Exp1 a = Exp (a -> a)
type Exp2 a = Exp (a -> a -> a)
type Exp3 a = Exp (a -> a -> a -> a)


data Exp a where
  Exp :: Exp1 a
  Log :: Exp1 a

  Pow :: Exp2 a
  Cos     :: Exp1 a
  Sin     :: Exp1 a
  Tan     :: Exp1 a


  Sinh :: Exp1 a
  Cosh :: Exp1 a
  Tanh :: Exp1 a

  ASinh :: Exp1 a
  ACosh :: Exp1 a
  ATanh    :: Exp1 a

  Add :: Exp2 a
  Mul :: Exp2 a
  Sub :: Exp2 a
  Div :: Exp2 a

  Negate  :: Exp1 a
  Signum :: Exp1 a

  Clamp :: Exp3 a
  Min :: Exp2 a
  Max :: Exp2 a

  Lit :: PrimTy a -> a -> Exp1 a

  -- Add
  --
  -- ApplyConstant :: PrimTy a -> ScalarOp (a -> a -> a) a -> a -> ScalarOp (a -> a)
  --

data FreeA f a b where
    Pure :: (a -> b) -> FreeA f a b
    Embed :: f a b -> FreeA f a b
    Seq  :: FreeA f a b -> FreeA f b c -> FreeA f a c
    Par  :: FreeA f a b -> FreeA f a' b' -> FreeA f (a, a') (b, b')

--
data Op :: Type -> Type -> Type where
  Zip     :: Exp2 a    -> (Node ds a, Node ds a) `Op` Node ds a
  Map     :: Exp1 a -> Node ds a `Op` Node ds a

  Concat   :: SNat d -> (Node xs a, Node ys a) `Op` Node (Concats d xs ys) a
  Linear   :: SNat o -> Node '[i] a `Op` Node '[o] a

  --Reduce :: Exp2 a -> SNat d -> Node (Reduces d ds) a

  -- Reshape :: NatList ns ->
  -- Sum ::


  --Transpose :: SNat d -> SNat d' -> Node ds a `Op` Node (Transpose d d' ds) a
  --Permute ::

instance Category (FreeA f) where
  id    = Pure id
  (.)   = flip Seq

instance Arrow (FreeA f) where
  arr     = Pure
  first f = Par f id
  second  = Par id
  (***)   = Par


newtype (~>) a b = Net { unNet :: FreeA Op a b }

op2 :: Op a b -> a ~> b
op2 = Net . Embed
--
-- add :: (Node ds a, Node ds a) ~> Node ds a
-- add = op2 Add
--
-- sub :: (Node ds a, Node ds a) ~> Node ds a
-- sub = op2 Sub
--
-- mul :: (Node ds a, Node ds a) ~> Node ds a
-- mul = op2 Mul

-- negate :: Node ds a ~> Node ds a
-- negate
