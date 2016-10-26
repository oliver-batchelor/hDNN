{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-}



module Exp where

import Data.Maybe

import GHC.TypeLits
--import GHC.TypeLits.List
import Data.Kind (Type)
import Data.Proxy

import Type.Family.List
import Data.Type.Index

import Data.Promotion.Prelude.List hiding (Sum, Elem)
import HDNN.Prelude

import Data.Typeable

import Prelude ((.))
import Data.Singletons.TH
--import Data.Type.Sum

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

newtype Rev b a = Rev (a -> b)

elim :: FList (Rev a) xs -> Sum xs -> a
elim (Rev f :< fs) (L x)  = f x
elim (_ :< fs)     (R xs) = elim fs xs

infixr 5 :<

test, test1 :: Sum [Int, Double, Maybe String]
test = inj (3 :: Int)
test1 = inj (Just "fooobar")

f = (Rev show :< Rev show :< Rev show :< FNil)
res = (elim f test1, elim f test)

-- elim (Fun (f :: x1 -> a) :< fs) o@(OneOf (x :: x2)) = case eqT of
--   Just (Refl :: x1 :~: x2) -> f x
--   Nothing -> elim fs o



data Exp a where
    Op      ::  Op e -> Exp a
    App     ::  (Typed a, Typed b) => Exp (a -> b) -> Exp a -> Exp b
    Let     :: Exp a -> (Exp a -> Exp b) -> Exp b







op1 :: (Typed a, Typed b) => Op (a -> b) -> Exp a -> Exp b
op1 op a = Op op `App` a
--
op2 :: (Typed a, Typed b, Typed c) => Op (a -> b -> c) -> Exp a -> Exp b -> Exp c
op2 op a b = Op op `App` a `App` b

expand :: (Prim a, KnownNats ns, KnownNat n, n ~ Product ns) => Exp (Vector n a) -> Exp (Tensor ns a)
expand = op1 (Reshape natsList)
--
-- input :: (Prim a, KnownNats ns, KnownNat n, n ~ Product ns) => Exp (Tensor ns a)
-- input = expand (Op Input)
--
--
-- linear :: forall m n. (KnownNat m, KnownNat n) => Exp (Matrix' n m) -> Exp (Vector' n) -> Exp (Vector' m)
-- linear = op2 (Linear (snat @ m))
--
-- linear' ::  forall m n hs. (KnownNat m, KnownNat n) => Exp (Vector' n) -> Exp (Vector' m)
-- linear'  = linear input
--
--
-- y :: forall m n. (KnownNat n, KnownNat m) => Exp  (Vector' n -> Vector' m)
-- y = Lam $ linear' @m . linear' @m . linear' @20


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
