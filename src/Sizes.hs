{-# LANGUAGE  HetMet, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances, RankNTypes, ScopedTypeVariables, TypeFamilies, TemplateHaskell, QuasiQuotes, GADTs, DataKinds, KindSignatures, TypeInType #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Sizes where

import Data.Singletons.TypeLits
import Data.Kind (Type)
import Data.Proxy

import Data.Maybe

import Data.Singletons
import Data.Singletons.TH

import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe

import Data.Singletons.Prelude


import GHC.TypeLits


-- data Op (t :: [Nat] -> *) a b where
--   Concat :: KnownNat i => Proxy i -> Op (t d, t d') (t (Concats i d d'))
--   Add    :: Op (t d, t d) (t d)
--   Mul    :: Op (t d, t d) (t d)
--
--   Linear :: KnownNat m => Op (t [n]) (t [m])
--   Conv :: (KnownNat w, KnownNat h) => Proxy Op (t [
--
--   Add' :: Double -> Op (t d) (t d)





-- promote [d|
--   matches :: (Eq a, Num a) => a -> [a] -> [a] -> Bool
--   matches i (x:xs) (y:ys)
--     | i == 0    = xs == ys
--     | otherwise = x == y && matches (i - 1) xs ys
--   matches _ _ _ = False
--
--   fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
--   fmapMaybe _ Nothing = Nothing
--   fmapMaybe f (Just a) = Just (f a)
--
--
--   concatShape  :: (Eq a, Num a, Eq i, Num i) => i -> [a] -> [a] -> Maybe [a]
--   concatShape i (x:xs) (y:ys)
--     | i == 0 = if xs == ys then Just (x + y : xs) else Nothing
--     | otherwise = if x == y then fmapMaybe (x :) (concatShape (i - 1) xs ys) else Nothing
--   concatShape _ _ _ = Nothing
--
--
--   |]
--
--
-- type family Concats d xs ys where
--   Concats d xs ys = FromMaybe (TypeError (Text "Cannot concat dimension " :<>: ShowType d :<>: Text " for ("
--     :<>: ShowType xs :<>: Text ", " :<>: ShowType ys :<>: Text ")")) (ConcatShape d xs ys)
--
--
-- con :: (KnownNat d) => Proxy (d :: Nat) -> Expr xs -> Expr ys -> Expr (Concats d xs ys)
-- con d = undefined where
--   k = natVal d
--



  {-
con1 :: Expr (x:'-}

-- test :: Expr '[3, 3, a] -> Expr '[3, 3, b] -> Expr '[3, 3, a :+ b]
-- test x y = con (Proxy :: Proxy 2) x y
-- --
-- x = Input :: Expr '[2,2,2]
-- y = Input :: Expr '[2,4,2]
--
-- z = con (Proxy :: Proxy 1) x y
-- zz = con (Proxy :: Proxy 0) x y

