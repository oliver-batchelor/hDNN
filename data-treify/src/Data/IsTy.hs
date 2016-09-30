{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleContexts,
    UndecidableInstances, KindSignatures, ConstraintKinds,
    TypeOperators, MultiParamTypeClasses, TypeFamilies,
    FlexibleInstances, ScopedTypeVariables, Rank2Types #-}
    
module Data.IsTy
  ( Yes, Yes2, IsTy(..), IsTy2(..), (:~:)(..)
  ) where


import Data.Type.Equality
import GHC.Exts (Constraint)

class Yes (f :: * -> *) a
instance Yes f a

-- | Type class for typed type representations
class IsTy f where
  type IsTyConstraint f z :: Constraint
  type IsTyConstraint f z = Yes f z
  tyEq :: (IsTyConstraint f a, IsTyConstraint f b) =>
          f a -> f b -> Maybe (a :~: b)

class Yes2 (f :: * -> * -> *) a b
instance Yes2 f a b

-- | Type class for typed type representations
class IsTy2 f where
  type IsTy2Constraint f u v :: Constraint
  type IsTy2Constraint f u v = Yes2 f u v
  tyEq2 :: (IsTy2Constraint f a b, IsTy2Constraint f c d) =>
           f a b -> f c d -> Maybe ((a,b) :~: (c,d))
