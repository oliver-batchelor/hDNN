{-# LANGUAGE TemplateHaskell #-}

module HDNN.Type.Operations where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Maybe
import Prelude hiding ( take )

import GHC.TypeLits (Nat, TypeError, ErrorMessage(..))

$(singletons [d|

  fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
  fmapMaybe f Nothing = Nothing
  fmapMaybe f (Just a) = Just (f a)


  maybeConcat :: (Num i, Eq i) => i -> [i] -> [i] -> Maybe [i]
  maybeConcat d (x:xs) (y:ys)
      | d == 0 && xs == ys  = Just (x + y : xs)
      | x == y  =  (x:) `fmapMaybe` maybeConcat (d - 1) xs ys
      | otherwise = Nothing

  |])



class CanConcat (i :: Nat) (ds :: [Nat]) (ds' :: [Nat]) where
  type ConcatDim i ds ds' :: [Nat]
  type ConcatDim i ds ds' = TypeError (Text "foo")


instance  CanConcat i ds ds' where
  type ConcatDim i ds ds' = FromJust (MaybeConcat i ds ds')


data T ds = T

foo :: (CanConcat 1 ds ds') => T ds -> T ds' -> T (ConcatDim 1 ds ds')
foo = undefined

x :: T [1,2,3]
x = T

y :: T [2,2,3]
y = T

z = foo x y
