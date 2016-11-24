
module SumF where

import GHC.Generics (Generic1)

import Data.Kind (Type, Constraint)
import Data.Proxy

import Data.Functor.Classes

import Type.Family.List
import Data.Type.Index
import Data.Type.Remove

import Type.Class.Witness
import Prelude

import Control.Arrow
import Data.Either

import Text.Show.Deriving
import Data.Singletons.Prelude.List hiding (Elem)


data SumF :: [k -> Type] -> k -> Type where
  L :: f a -> SumF (f : fs) a
  R :: SumF fs a -> SumF (f : fs) a


inj :: (Elem fs f) => f a -> SumF fs a
inj = inj' elemIndex


inj' :: Index fs f -> f a -> SumF fs a
inj' = \case
  IZ   -> L
  IS x -> R . inj' x

class Proj fs f where
  proj :: SumF fs a -> Maybe (f a)

instance Proj (f:fs) f where
  proj (L x) = Just x
  proj _     = Nothing

instance Proj fs g => Proj (f:fs) g where
  proj (R ys) = proj ys

elim :: forall c fs a r. (Every c fs) => (forall f. Wit (c f) -> f a -> r) -> SumF fs a -> r
elim f (L x)  = f Wit x
elim f (R fs) = elim f fs


showsPrec1' :: (Show a) => Wit (Show1 f) -> Int -> f a -> ShowS
showsPrec1' Wit d x = showParen (d > 10) $ showsPrec1 11 x

instance (Every Show1 fs, Show a) => Show (SumF fs a) where
  showsPrec n = elim (flip showsPrec1' n)

instance (Every Functor fs) => Functor (SumF fs) where
  fmap f (L x)  = L (fmap f x)
  fmap f (R fs) = R (fmap f fs)

data Foo a = Foo a deriving (Generic1, Functor)
deriveShow1 ''Foo

data Params a = Params a deriving (Generic1, Functor)
deriveShow1 ''Params


type Types = [Maybe, Foo, Either String]



type Types' = Params ': Types
--
mkFoo :: (Elem Types' f) => f a -> SumF Types' a
mkFoo = inj
--
test :: SumF Types' Int
test = mkFoo (Foo 3)

test1 :: SumF Types' String
test1 = mkFoo (Just "fooobar")

param1 :: SumF Types' String
param1 = mkFoo (Params "fooobar")



gone = replace (\(Params s) -> Just s) param1


replace ::  (Without fs f gs, Elem gs g) => (f a -> g a) -> SumF fs a -> SumF gs a
replace f = either (inj . f) id . remove

remove ::  Without fs f gs => SumF fs a -> Either (f a) (SumF gs a)
remove  = remove' without

remove' ::  Remove fs f gs -> SumF fs a -> Either (f a) (SumF gs a)
remove'  RZ (L x) = Left x
remove'  RZ (R xs) = Right xs
remove'  (RS n) (L x) = Right (L x)
remove'  (RS n) (R xs) = right R (remove' n xs)
