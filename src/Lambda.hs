--{-# OPTIONS_GHC -ddump-splices #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Examples.MultiParam.Lambda
-- Copyright   :  (c) 2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Tom Hvitved <hvitved@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Tagless (monadic) interpretation of extended lambda calculus
--
--------------------------------------------------------------------------------

module Lambda where

import Data.Comp.Param.Multi
import Data.Comp.Param.Multi.FreshM


import Data.Comp.Param.Multi.Show ()
import Data.Comp.Param.Multi.Equality ()
import Data.Comp.Param.Multi.Derive
import Control.Monad (liftM2)
import Control.Monad.Except (MonadError, throwError)

import Data.Kind (Type)
import Data.Char (isSpace)
import Prelude

type F = (Type -> Type) -> (Type -> Type) -> (Type -> Type)

data Lam :: F where
  Lam :: (var i -> exp j) -> Lam var exp (i -> j)

data App :: F where
  App :: exp (i -> j) -> exp i -> App var exp j

data Let :: F where
  Let :: exp i -> (var i -> exp j) -> Let var exp j

data Const :: F where
  Const :: Int -> Const var exp Int

data Plus :: F where
  Plus :: exp Int -> exp Int -> Plus var exp Int


data Id :: F where
  Id :: exp i -> Id var exp i

data Err :: F where
  Err :: Err var exp i

type Sig = Lam :+: App :+: Const :+: Plus :+: Err :+: Let


brackets :: String -> String
brackets str | any isSpace str = "(" ++ str ++ ")"
             | otherwise = str

instance ShowHD Const where
  showHD (Const i) = return (show i)

instance ShowHD Plus where
  showHD (Plus a b) =
     (\a' b' -> unwords [a', "+", b']) <$> unK a <*> unK b

instance ShowHD Lam where
 showHD (Lam f) = withName $ \name -> do
   f' <- unK (f name)
   return $ unwords ["\\" ++ show name, "->", f']

instance ShowHD Let where
  showHD (Let e f) = withName $ \name -> do
    f' <- unK (f name)
    e' <- unK e
    return $ unwords ["let", show name, "=", e', "in", f']


instance ShowHD App where
  showHD (App f a) =
    (\f a -> unwords [brackets f, brackets a]) <$> unK f <*> unK a


$(derive [makeShowHD]
    [''Err])

$(derive [smartConstructors, makeHDifunctor, makeEqHD]
  [''Lam, ''App, ''Const, ''Plus, ''Err, ''Let])



-- Type Tagless interpretation
class Eval f where
  evalAlg :: f I I i -> i -- I . evalAlg :: Alg f I is the actual algebra

$(derive [liftSum] [''Eval])

eval :: (HDifunctor f, Eval f) => Term f i -> i
eval = unI . cata (I . evalAlg)

instance Eval Lam where
  evalAlg (Lam f) = unI . f . I

instance Eval App where
  evalAlg (App (I f) (I x)) = f x

instance Eval Const where
  evalAlg (Const n) = n

instance Eval Plus where
  evalAlg (Plus (I x) (I y)) = x + y

instance Eval Err where
  evalAlg Err = error "error"

-- Type Tagless monadic interpretation
type family Sem (m :: Type -> Type) i
type instance Sem m (i -> j) = Sem m i -> m (Sem m j)
type instance Sem m Int = Int

newtype M m i = M {unM :: m (Sem m i)}

class Monad m => EvalM m f where
  evalMAlg :: f (M m) (M m) i -> m (Sem m i) -- M . evalMAlg :: Alg f (M m)

$(derive [liftSum] [''EvalM])

evalM :: (Monad m, HDifunctor f, EvalM m f) => Term f i -> m (Sem m i)
evalM = unM . cata (M . evalMAlg)

instance Monad m => EvalM m Lam where
  evalMAlg (Lam f) = return $ unM . f . M . return

instance Monad m => EvalM m Let where
  evalMAlg (Let (M mi) f) = unM . f . M . return =<< mi

instance Monad m => EvalM m App where
  evalMAlg (App (M mf) (M mx)) = do f <- mf; f =<< mx

instance Monad m => EvalM m Const where
  evalMAlg (Const n) = return n

instance Monad m => EvalM m Plus where
  evalMAlg (Plus (M mx) (M my)) = liftM2 (+) mx my

instance (Monad m, MonadError String m) => EvalM m Err where
  evalMAlg Err = throwError "error" -- 'throwError' rather than 'error'


-- instance Num (Term Sig Int) where
--   fromInteger = iConst . fromInteger

instance (Const :<: f, Plus :<: f) => Num (Cxt h f a b Int) where
  fromInteger = iConst . fromInteger
  (+) = iPlus

instance (Const :<: f, Plus :<: f) => Num (Term f Int) where
    fromInteger x = Term (iConst $ fromInteger x)
    (+) (Term a) (Term b) = Term $ a `iPlus` b


--
--
-- e :: Trm Sig Int
-- e = iLet (iLam (\x -> iLam (+ x) `iApp` 3) `iApp` 2) (\k -> (k + k))
--
--
-- v :: Either String Int
-- v = evalM e
--
-- e' :: Term Sig (Int -> Int)
-- e' = Term iErr --(iLam id)
--
-- v' :: Either String (Int -> Either String Int)
-- v' = evalM e'
