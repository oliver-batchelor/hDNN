{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-}



module Exp where

import Data.Maybe

import GHC.TypeLits
--import GHC.TypeLits.List
import Data.Kind (Type)
import Data.Proxy

import Type.Family.List
import Data.Promotion.Prelude.List
import HDNN.Prelude

import Prelude ((.))
import Data.Singletons.TH
--import Data.Singletons.TypeLits

import HDNN.Typed
import HDNN.Tensor

import HDNN.TGraph

import Control.Monad.State


type Tensor' ds = Tensor ds Float
type Vector' n = Vector n Float
type Matrix' n m = Matrix n m Float

--
data Op a where
   Linear :: SNat m -> Op (Matrix' n m  -> Vector' n  -> Vector' m)
--   --Flatten :: KnownNats ns => Tensor ns a -> Vector (Product ns) a
   Reshape :: (Product ns ~ Product ns') => NatList ns' -> Op (Tensor ns a -> Tensor ns' a)
   Input   :: Typed a => Op a
--   Map :: (forall a. Floating a => a -> a) -> Op (Tensor' ns ~> Tensor' ns)
--   Zip :: (forall a. Floating a => a -> a -> a) -> Op (Tensor' ns ~> Tensor' ns ~> Tensor' ns)


data Exp  a where
    Op      ::  Op a -> Exp  a
    (:$)    ::  (Typed a, Typed b) => Exp (a -> b) -> Exp a -> Exp b
    Lam     :: (Exp  a -> Exp b) -> Exp  (a -> b)
    Let     :: Exp a -> (Exp a -> Exp b) -> Exp b





op1 :: (Typed a, Typed b) => Op (a -> b) -> Exp a -> Exp b
op1 op a = Op op :$ a
--
op2 :: (Typed a, Typed b, Typed c) => Op (a -> b -> c) -> Exp a -> Exp b -> Exp c
op2 op a b = Op op :$ a :$ b

expand :: (Prim a, KnownNats ns, KnownNat n, n ~ Product ns) => Exp (Vector n a) -> Exp (Tensor ns a)
expand = op1 (Reshape natsList)

input :: (Prim a, KnownNats ns, KnownNat n, n ~ Product ns) => Exp (Tensor ns a)
input = expand (Op Input)


linear :: forall m n. (KnownNat m, KnownNat n) => Exp (Matrix' n m) -> Exp (Vector' n) -> Exp (Vector' m)
linear = op2 (Linear (snat @ m))

linear' ::  forall m n hs. (KnownNat m, KnownNat n) => Exp (Vector' n) -> Exp (Vector' m)
linear'  = linear input


y :: forall m n. (KnownNat n, KnownNat m) => Exp  (Vector' n -> Vector' m)
y = Lam $ linear' @m . linear' @m . linear' @20


-- type Builder a = State [Bind Ty N] a
--
-- data N :: (Type -> Type) -> Type -> Type where
--    OpNode  :: Op a -> N v a
--    App     :: (Typed a, Typed b) => v (a -> b) -> v a -> N v b
--
--
-- data GraphFun a b = (~>) (V Ty a)  (Graph Ty N b)
--
-- bind :: forall a. Typed a => N (V Ty) a -> Builder (V Ty a)
-- bind n = do
--   binds <- get
--   let v = V (length binds) ty
--   modify (Bind v n :)
--   return v
--
-- toGraph :: Typed a => Exp a -> Builder (V Ty a)
-- toGraph (Op op) = bind (OpNode op)
-- toGraph (f :$ a) = case f of
--   (Lam g) -> toGraph (g a)
--   _       -> bind =<< App <$> toGraph f <*> toGraph a
--toGraph (Lam g) = toGraph $ g (Op Input)

  --App <$> toGraph f <*> toGraph a >>= bind




-- (:$)    ::  Exp (a -> b) -> Exp a -> Exp b
-- Lam     :: (Exp  a -> Exp b) -> Exp  (a -> b)

instance ShowF Op where
  showF (Linear (SNat m)) = unwords ["Linear ", show (natVal m)]
  showF (Reshape (n :<# ns)) = unwords ["Reshape ", show (natVal n : natsVal ns)]

instance ShowF v => ShowF (N v) where
   showF (OpNode o)    = unwords ["Op", showF o]
   showF (App a b)     = unwords ["App",showF a,showF b]
