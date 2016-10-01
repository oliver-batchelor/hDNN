{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-}


-- Example of data-treify for a first pass of CSE on a simple typed
-- language representation.

module HDNN.Exp where


import Control.Applicative (pure,(<$>),(<*>))
import qualified Data.IntMap as I
import Data.Maybe (fromMaybe) -- ,fromJust

-- package ty
import Data.Type.Equality

-- Import one of the following but not both
--import Data.Ty
import HDNN.Typed
import HDNN.Tensor

import HDNN.Type.Shape
import HDNN.Prelude

import Data.TReify (MuRef(..),ShowF(..),V(..),Graph(..),Bind(..),Id,reifyGraph)

{--------------------------------------------------------------------
    Expressions
--------------------------------------------------------------------}

data Op :: Type -> Type where
  Add :: (IsPrim a, Num a) => Op (Tensor ds a -> Tensor ds a -> Tensor ds a)
  Mul :: (IsPrim a, Num a) => Op (Tensor ds a -> Tensor ds a -> Tensor ds a)
  Cat0 :: (IsPrim a) => Op (Tensor (d0 ': ds) a -> Tensor (d0' : ds') a -> Tensor (d0 + d0' : ds) a)

  Cat :: SNat d -> Op (Tensor ds a -> Tensor ds' a -> Tensor (ConcatShape d ds ds') a)
  Lit :: (Show a, KnownNats ds) => Tensor ds a -> Op (Tensor ds a)

instance ShowF Op where
  showF Add = "Add"
  showF Mul = "Mul"
  showF (Cat _) = "Cat"
  showF (Lit a) = show a

instance Show (Op a) where show = showF

-- Expressions, parameterized by variable type (constructor) and
-- expression type.
data E :: Type -> Type where
  Op   :: Op a -> E a
  (:$) :: (Typed a, Typed b) =>
          E (a -> b) -> E a -> E b
--  Let  :: v a -> E v a -> E v b -> E v b
--  Var  :: v a -> E v a

data N :: (Type -> Type) -> Type -> Type where
  On  :: Op a -> N v a
  App :: (Typed a, Typed b) =>
         v (a -> b) -> v a -> N v b

instance ShowF v => ShowF (N v) where
  showF (On o)    = unwords ["ON" ,showF o]
  showF (App a b) = unwords ["App",showF a,showF b]

-- instance Show (E (V Ty) a) where
--   show (Op o) = show o
--   show (u :^ v) = parens $ unwords [show u,show v]
--   show (Let v a b) = unwords ["let",show v,"=",show a,"in",show b]
--   show (Var v) = show v

instance Show (E a) where
  show (Op o) = show o
  show (u :$ v) = parens $ unwords [show u,show v]
  -- show (Let v a b) = unwords ["let",showF v,"=",show a,"in",show b]
  -- show (Var v) = showF v

parens :: String -> String
parens = ("(" ++) . (++ ")")

-- TODO: showsPrec with infix and minimal parens


instance MuRef Ty E where
  type DeRef E = N

  mapDeRef _ _ (Op o)   = pure $ On  o
  mapDeRef k _ (f :$ a) = App <$> k ty f <*> k ty a
  -- mapDeRef _ _ Let{}    = notSupp "Let"
  -- mapDeRef _ _ Var{}    = notSupp "Var"


-- TODO: Consider splitting Let/Var off from E.  Then E wouldn't need the
-- v parameter.
--
-- I could use N and Mu to define an E without Let & Var and then use a
-- standard MuRef instance and a standard extension to include Let & Var.
-- Wouldn't be as convenient for simplification rules, because of the
-- explicit Mu constructors.  Consider it, however, since it makes a
-- simple type distinction between the input to CSE and the output.  Oh,
-- and I could have different ways to embed Let.  For C-like languages, I
-- could restrict the lets to be on the outside (odd for conditionals,
-- though).


-- nodeE :: N v a -> E v a
-- nodeE (ON o)    = Op o
-- nodeE (App u v) = Var u :^ Var v

-- unGraph :: Typeable a => Graph Ty N a -> E (V Ty) a
-- unGraph (Graph binds root) =
--   foldr (\ (Bind v n) -> Let v (nodeE n)) (Var root) (reverse binds)


-- Convert expressions to simple SSA forms
-- ssa :: Typeable a => E (V Ty) a -> IO (E (V Ty) a)
-- ssa = fmap unGraph . reifyGraph ty



children :: N (V Ty) a -> [Id]
children (On  _)   = []
children (App (V a _) (V b _)) = [a,b]

childrenB :: Bind Ty N -> [Id]
childrenB (Bind _ n) = children n


-- Number of references for each node.  Important: partially apply, so
-- that the binding list can be converted just once into an efficiently
-- searchable representation.
uses :: [Bind Ty N] -> (Id -> Int)
uses = fmap (fromMaybe 0) .
       flip I.lookup .
       histogram .
       concatMap childrenB

-- histogram :: Ord k => [k] -> I.Map k Int
-- histogram = foldr (\ k -> I.insertWith (+) k 1) I.empty

histogram :: [Int] -> I.IntMap Int
histogram = foldr (\ k -> I.insertWith (+) k 1) I.empty

-- bindsF :: [Bind v] -> (Ty a -> v a -> N v a)
-- bindsF = undefined

-- Slow version
bindsF' :: [Bind Ty N] -> (V Ty a -> N (V Ty) a)
bindsF' [] _ = error "bindsF: ran out of bindings"
bindsF' (Bind (V i a) n : binds') v@(V i' a')
  | Just Refl <- a `tyEq` a', i == i' = n
  | otherwise                         = bindsF' binds' v

-- Fast version, using an IntMap.  Important: partially apply.
bindsF :: forall n a. [Bind Ty n] -> (V Ty a -> n (V Ty) a)
bindsF binds = \ (V i' a') -> extract a' (I.lookup i' m)
 where
   m :: I.IntMap (Bind Ty n)
   m = I.fromList [(i,b) | b@(Bind (V i _) _) <- binds]
   extract :: Ty a' -> Maybe (Bind Ty n) -> n (V Ty) a'
   extract _ Nothing            = error "bindsF: variable not found"
   extract a' (Just (Bind (V _ a) n))
     | Just Refl <- a `tyEq` a' = n
     | otherwise                = error "bindsF: wrong type"


-- TODO: generalize unGraph2 from V.



{--------------------------------------------------------------------
    Utilities for convenient expression building
--------------------------------------------------------------------}

op2 :: (Typed a, Typed b, Typed c) =>
       Op (a -> b -> c) -> E a -> E b -> E c
op2 h a b = Op h :$ a :$ b


op1 :: (Typed a, Typed b) =>
       Op (a -> b) -> E a -> E b
op1 h a = Op h :$ a


instance (Show a, Num a, IsPrim a, KnownNats ds) => Num (E (Tensor ds a)) where
  fromInteger = Op . Lit . fromInteger
  (+) = op2 Add
  (*) = op2 Mul
  (-) = undefined
  abs = undefined
  signum = undefined
  negate = undefined


cat :: (IsPrim a, KnownNats (ConcatShape d ds ds'), KnownNats ds, KnownNats ds')
  => SNat d -> E (Tensor ds a) -> E (Tensor ds' a) -> E (Tensor (ConcatShape d ds ds') a)
cat d = op2 (Cat d)

cat0 :: (IsPrim a, KnownNat d0, KnownNat d0', KnownNats ds, ds ~ ds')
  => E (Tensor (d0 : ds) a) -> E (Tensor (d0' : ds') a) -> E (Tensor (d0 + d0' : ds) a)
cat0 = op2 Cat0


sqr :: Num a => a -> a
sqr x = x * x

{--------------------------------------------------------------------
    Testing
--------------------------------------------------------------------}

-- type-specialize
reify :: (MuRef Ty h, Typed a) => h a -> IO (Graph Ty (DeRef h) a)
reify = reifyGraph ty

-- test expressions
e1 = 3 + 5 :: E (Vector 1 Float)
e2 = e1 * e1 * e1
e3 = 3 + 3 :: E (Vector 1 Float)

e4 = cat (snat @ 0) e1 e2

mat = 3 + 5 :: E (Matrix 3 3 Float)
--e5 = cat (Proxy :: Proxy 0) mat e4


--
-
