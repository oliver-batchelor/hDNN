
module Sum where


import Data.Kind (Type, Constraint)
import Data.Proxy

import Type.Family.List
import Data.Type.Index

import Type.Class.Witness
import Prelude



data Sum :: [Type] -> Type where
  L :: x -> Sum (x : xs)
  R :: Sum xs -> Sum (x : xs)


inj :: (Elem xs x) => x -> Sum xs
inj = inj' elemIndex


inj' :: Index xs x -> x -> Sum xs
inj' = \case
  IZ   -> L
  IS x -> R . inj' x

class Proj x xs where
  proj :: Proxy x -> Sum xs -> Maybe x

instance Proj x (x:xs) where
  proj _ (L x) = Just x
  proj _ _     = Nothing

instance Proj x ys => Proj x (y:ys) where
  proj p (R ys) = proj p ys

data FList f xs where
  FNil :: FList f '[]
  (:<) ::  f x -> FList f xs -> FList f (x : xs)


data F c a = F (forall x. c x => x -> a)

elim :: forall c xs a. (Every c xs) => (forall x. Wit (c x) -> x -> a) -> Sum xs -> a
elim f (L x)  =  f Wit x
elim f (R xs) = elim f xs




instance (Every Show xs) => Show (Sum xs) where
  show = elim showWit

infixr 5 :<

data Foo = Foo deriving Show

type Types = [Int, Double, Maybe String, Foo]

mkFoo :: (Elem Types x) => x -> Sum Types
mkFoo = inj

test, test1 :: Sum Types
test = inj (3 :: Int)
test1 = inj (Just "fooobar")

showWit :: Wit (Show x) -> x -> String
showWit Wit = show

res = (elim showWit test1, elim showWit test)
