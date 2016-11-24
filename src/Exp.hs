{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-}



module Exp where

import Data.Maybe

import GHC.TypeLits
--import GHC.TypeLits.List
import Data.Kind (Type, Constraint)
import Data.Proxy

import Type.Family.List
import Data.Type.Index

import Type.Class.Witness

import Data.Promotion.Prelude.List hiding (Sum, Elem)
import HDNN.Prelude

import Data.Typeable

import Prelude ((.))
import Data.Singletons.TH
import Data.Singletons.Prelude
--import Data.Type.Sum

import Data.Singletons.TypeLits

import SumF
import HDNN.Tensor

import Control.Monad.State



type Vector (n :: Nat) a = Tensor '[n] a
type Matrix n m a = Tensor '[n, m] a

--
-- data Op a where
--    Linear :: SNat m -> Op (Matrix n m F :-> Vector n F :-> Full (Vector m F))
--   --Flatten :: KnownNats ns => Tensor ns a -> Vector (Product ns) a
  --  Reshape :: (Product ns ~ Product ns') => NatList ns' -> Op (Tensor ns a -> Tensor ns' a)
  --  Input   :: Op a
--   Map :: (forall a. Floating a => a -> a) -> Op (Tensor' ns ~> Tensor' ns)
--   Zip :: (forall a. Floating a => a -> a -> a) -> Op (Tensor' ns ~> Tensor' ns ~> Tensor' ns)

--
-- data AST :: (Sig t -> Type) -> Sig t -> Type where
--   Sym  :: dom sig -> AST dom sig
--   (:$) :: AST dom (a :-> sig) -> AST dom (Full a) -> AST dom sig
--   Let  :: AST dom (Full a) -> (AST dom (Full a) -> AST dom (Full b)) -> AST dom (Full b)
--
-- infixl 1 :$
-- infixr :*
--
-- type ASTF dom a = AST dom (Full a)
--
--
-- typedFold :: forall dom f b. (forall a. dom a -> Args f a -> f (Full (Result a))) ->
--            ASTF dom b -> f (Full b)
-- typedFold f e = go e NilA
--   where
--     go :: forall a. AST dom a -> Args f a -> f (Full (Result a))
--     go (Sym s)     args = f s args
--     go (Let a g)   args =
--     go (s :$ arg)  args = go s (typedFold f arg :* args)
--
-- everywhere :: (forall a. ASTF dom a -> ASTF dom a) ->
--               (forall a. ASTF dom a -> ASTF dom a)
-- everywhere f = typedFold (\s -> f . appArgs (Sym s))
--     appArgs :: AST dom sig -> Args (AST dom) sig -> ASTF dom (Result sig)
--     appArgs a Nil = a
--     appArgs s (a :* as) = appArgs (s :$ a) as
--
--
-- foo :: Sing (a :: T Nat) -> String
-- foo (STensor xs t) = case xs of
--       (SNat :: (Sing n) ) `SCons` xs -> show $ natVal (Proxy @n)

--foo :: SingI (Vector n a) => Proxy




-- elim (Fun (f :: x1 -> a) :< fs) o@(OneOf (x :: x2)) = case eqT of
--   Just (Refl :: x1 :~: x2) -> f x
--   Nothing -> elim fs o

--
--
-- data AST a where
--     Op      ::  Op e -> AST a
--     App     ::  (Typed a, Typed b) => AST (a -> b) -> AST a -> AST b
--     Let     :: AST a -> (AST a -> AST b) -> AST b
--
--
--
--
--
--
--
-- op1 :: (Typed a, Typed b) => Op (a -> b) -> AST a -> AST b
-- op1 op a = Op op `App` a
-- --
-- op2 :: (Typed a, Typed b, Typed c) => Op (a -> b -> c) -> AST a -> AST b -> AST c
-- op2 op a b = Op op `App` a `App` b
--
-- ASTand :: (Prim a, KnownNats ns, KnownNat n, n ~ Product ns) => AST (Vector n a) -> AST (Tensor ns a)
-- ASTand = op1 (Reshape natsList)
-- --
-- input :: (Prim a, KnownNats ns, KnownNat n, n ~ Product ns) => AST (Tensor ns a)
-- input = ASTand (Op Input)
--
--
-- linear :: forall m n. (KnownNat m, KnownNat n) => AST (Matrix' n m) -> AST (Vector' n) -> AST (Vector' m)
-- linear = op2 (Linear (snat @ m))
--
-- linear' ::  forall m n hs. (KnownNat m, KnownNat n) => AST (Vector' n) -> AST (Vector' m)
-- linear'  = linear input
--
--
-- y :: forall m n. (KnownNat n, KnownNat m) => AST  (Vector' n -> Vector' m)
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
-- toGraph :: Typed a => AST a -> Builder (V Ty a)
-- toGraph (Op op) = bind (OpNode op)
-- toGraph (f :$ a) = case f of
--   (Lam g) -> toGraph (g a)
--   _       -> bind =<< App <$> toGraph f <*> toGraph a
--toGraph (Lam g) = toGraph $ g (Op Input)

  --App <$> toGraph f <*> toGraph a >>= bind




-- (:$)    ::  AST (a -> b) -> AST a -> AST b
-- Lam     :: (AST  a -> AST b) -> AST  (a -> b)
