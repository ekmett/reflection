{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
#define USE_TYPE_LITS 1
#endif
#ifdef MIN_VERSION_template_haskell
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Reflection
-- Copyright  : 2009-2015 Edward Kmett,
--              2012 Elliott Hird,
--              2004 Oleg Kiselyov and Chung-chieh Shan
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Reifies arbitrary terms at the type level. Based on the Functional
-- Pearl: Implicit Configurations paper by Oleg Kiselyov and
-- Chung-chieh Shan.
--
-- <http://okmij.org/ftp/Haskell/tr-15-04.pdf>
--
-- The approach from the paper was modified to work with Data.Proxy
-- and to cheat by using knowledge of GHC's internal representations
-- by Edward Kmett and Elliott Hird.
--
-- Usage comes down to two combinators, 'reify' and 'reflect'.
--
-- >>> reify 6 (\p -> reflect p + reflect p)
-- 12
--
-- The argument passed along by reify is just a @data 'Proxy' t =
-- Proxy@, so all of the information needed to reconstruct your value
-- has been moved to the type level.  This enables it to be used when
-- constructing instances (see @examples/Monoid.hs@).
--
-- In addition, a simpler API is offered for working with singleton
-- values such as a system configuration, etc.
-------------------------------------------------------------------------------
module Data.Reflection
    (
    -- * Reflection
      Reifies(..)
    , reify
#if __GLASGOW_HASKELL__ >= 708
    , reifyNat
    , reifySymbol
#endif
    -- * Given
    , Given(..)
    , give
#ifdef MIN_VERSION_template_haskell
    -- * Template Haskell reflection
    , int, nat
#endif
    -- * Useful compile time naturals
    , Z, D, SD, PD
    ) where

import Data.Proxy

#if (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707) || (defined(MIN_VERSION_template_haskell) && USE_TYPE_LITS)
import GHC.TypeLits
#endif

#ifdef MIN_VERSION_template_haskell
import Language.Haskell.TH hiding (reify)
import Control.Monad
#endif

#ifdef __HUGS__
import Hugs.IOExts
#else
import Unsafe.Coerce
#endif

------------------------------------------------------------------------------
-- Reifies
------------------------------------------------------------------------------

class Reifies s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its
  -- reified type.
  reflect :: proxy s -> a

newtype Magic a r = Magic (forall (s :: *). Reifies s a => Proxy s -> r)

-- | Reify a value at the type level, to be recovered with 'reflect'.
reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r
reify a k = unsafeCoerce (Magic k :: Magic a r) (const a) Proxy
{-# INLINE reify #-}

#if __GLASGOW_HASKELL__ >= 707
instance KnownNat n => Reifies n Integer where
  reflect = natVal

instance KnownSymbol n => Reifies n String where
  reflect = symbolVal
#endif

#if __GLASGOW_HASKELL__ >= 708
newtype MagicNat r = MagicNat (forall (n :: Nat). KnownNat n => Proxy n -> r)

-- | This upgraded version of 'reify' can be used to generate a 'KnownNat' suitable for use with other APIs.
--
-- /Available only on GHC 7.8+/
--
-- >>> reifyNat 4 natVal
-- 4
--
-- >>> reifyNat 4 reflect
-- 4

reifyNat :: forall r. Integer -> (forall (n :: Nat). KnownNat n => Proxy n -> r) -> r
reifyNat n k = unsafeCoerce (MagicNat k :: MagicNat r) n Proxy

newtype MagicSymbol r = MagicSymbol (forall (n :: Symbol). KnownSymbol n => Proxy n -> r)

-- | This upgraded version of 'reify' can be used to generate a 'KnownSymbol' suitable for use with other APIs.
--
-- /Available only on GHC 7.8+/
--
-- >>> reifySymbol "hello" symbolVal
-- "hello"
--
-- >>> reifySymbol "hello" reflect
-- "hello"
reifySymbol :: forall r. String -> (forall (n :: Symbol). KnownSymbol n => Proxy n -> r) -> r
reifySymbol n k = unsafeCoerce (MagicSymbol k :: MagicSymbol r) n Proxy
#endif

------------------------------------------------------------------------------
-- Given
------------------------------------------------------------------------------

-- | This is a version of 'Reifies' that allows for only a single value.
--
-- This is easier to work with than 'Reifies' and permits extended defaulting,
-- but it only offers a single reflected value of a given type at a time.
class Given a where
  -- | Recover the value of a given type previously encoded with 'give'.
  given :: a

newtype Gift a r = Gift (Given a => r)

-- | Reify a value into an instance to be recovered with 'given'.
--
-- You should /only/ 'give' a single value for each type. If multiple instances
-- are in scope, then the behavior is implementation defined.
give :: forall a r. a -> (Given a => r) -> r
give a k = unsafeCoerce (Gift k :: Gift a r) a
{-# INLINE give #-}

data Z -- 0
data D  (n :: *) -- 2n
data SD (n :: *) -- 2n+1
data PD (n :: *) -- 2n-1

instance Reifies Z Int where
  reflect _ = 0
  {-# INLINE reflect #-}

retagD :: (Proxy n -> a) -> proxy (D n) -> a
retagD f _ = f Proxy
{-# INLINE retagD #-}

retagSD :: (Proxy n -> a) -> proxy (SD n) -> a
retagSD f _ = f Proxy
{-# INLINE retagSD #-}

retagPD :: (Proxy n -> a) -> proxy (PD n) -> a
retagPD f _ = f Proxy
{-# INLINE retagPD #-}

instance Reifies n Int => Reifies (D n) Int where
  reflect = (\n -> n + n) `fmap` retagD reflect
  {-# INLINE reflect #-}

instance Reifies n Int => Reifies (SD n) Int where
  reflect = (\n -> n + n + 1) `fmap` retagSD reflect
  {-# INLINE reflect #-}

instance Reifies n Int => Reifies (PD n) Int where
  reflect = (\n -> n + n - 1) `fmap` retagPD reflect
  {-# INLINE reflect #-}

#ifdef MIN_VERSION_template_haskell
-- | This can be used to generate a template haskell splice for a type level version of a given 'int'.
--
-- This does not use GHC TypeLits, instead it generates a numeric type by hand similar to the ones used
-- in the \"Functional Pearl: Implicit Configurations\" paper by Oleg Kiselyov and Chung-Chieh Shan.
--
-- @instance Num (Q Exp)@ provided in this package allows writing @$(3)@
-- instead of @$(int 3)@. Sometimes the two will produce the same
-- representation (if compiled without the @-DUSE_TYPE_LITS@ preprocessor
-- directive).
int :: Int -> TypeQ
int n = case quotRem n 2 of
  (0, 0) -> conT ''Z
  (q,-1) -> conT ''PD `appT` int q
  (q, 0) -> conT ''D  `appT` int q
  (q, 1) -> conT ''SD `appT` int q
  _     -> error "ghc is bad at math"

-- | This is a restricted version of 'int' that can only generate natural numbers. Attempting to generate
-- a negative number results in a compile time error. Also the resulting sequence will consist entirely of
-- Z, D, and SD constructors representing the number in zeroless binary.
nat :: Int -> TypeQ
nat n
  | n >= 0 = int n
  | otherwise = error "nat: negative"

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 704
instance Show (Q a) where
  show _ = "Q"
instance Eq (Q a) where
  _ == _ = False
#endif
instance Num a => Num (Q a) where
  (+) = liftM2 (+)
  (*) = liftM2 (*)
  (-) = liftM2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger

instance Fractional a => Fractional (Q a) where
  (/) = liftM2 (/)
  recip = fmap recip
  fromRational = return . fromRational

-- | This permits the use of $(5) as a type splice.
instance Num Type where
#ifdef USE_TYPE_LITS
  LitT (NumTyLit a) + LitT (NumTyLit b) = LitT (NumTyLit (a+b))
  a + b = AppT (AppT (VarT ''(+)) a) b

  LitT (NumTyLit a) * LitT (NumTyLit b) = LitT (NumTyLit (a*b))
  (*) a b = AppT (AppT (VarT ''(*)) a) b
#if MIN_VERSION_base(4,8,0)
  a - b = AppT (AppT (VarT ''(-)) a) b
#else
  (-) = error "Type.(-): undefined"
#endif
  fromInteger = LitT . NumTyLit
#else
  (+) = error "Type.(+): undefined"
  (*) = error "Type.(*): undefined"
  (-) = error "Type.(-): undefined"
  fromInteger n = case quotRem n 2 of
      (0, 0) -> ConT ''Z
      (q,-1) -> ConT ''PD `AppT` fromInteger q
      (q, 0) -> ConT ''D  `AppT` fromInteger q
      (q, 1) -> ConT ''SD `AppT` fromInteger q
      _ -> error "ghc is bad at math"
#endif
  abs = error "Type.abs"
  signum = error "Type.signum"

onProxyType1 :: (Type -> Type) -> (Exp -> Exp)
onProxyType1 f
    (SigE _ ta@(AppT (ConT proxyName)  (VarT _)))
    | proxyName == ''Proxy = ConE 'Proxy `SigE` (ConT ''Proxy `AppT` f ta)
onProxyType1 f a =
        LamE [SigP WildP na] body `AppE` a
    where 
          body = ConE 'Proxy `SigE` (ConT ''Proxy `AppT` f na)
          na = VarT (mkName "na")

onProxyType2 :: Name -> (Type -> Type -> Type) -> (Exp -> Exp -> Exp)
onProxyType2 _fName f
    (SigE _ (AppT (ConT proxyName)  ta))
    (SigE _ (AppT (ConT proxyName') tb))
    | proxyName == ''Proxy,
      proxyName' == ''Proxy = ConE 'Proxy `SigE`
                                        (ConT ''Proxy `AppT` f ta tb)
-- the above case should only match for things like $(2 + 2)
onProxyType2 fName _f a b = VarE fName `AppE` a `AppE` b

-- | This permits the use of $(5) as an expression splice,
-- which stands for @Proxy :: Proxy $(5)@
instance Num Exp where
  (+) = onProxyType2 'addProxy (+)
  (*) = onProxyType2 'mulProxy (*)
  (-) = onProxyType2 'subProxy (-)
  negate = onProxyType1 negate
  abs = onProxyType1 abs
  signum = onProxyType1 signum
  fromInteger n = ConE 'Proxy `SigE` (ConT ''Proxy `AppT` fromInteger n)

#ifdef USE_TYPE_LITS
addProxy :: Proxy a -> Proxy b -> Proxy (a + b)
addProxy _ _ = Proxy
mulProxy :: Proxy a -> Proxy b -> Proxy (a * b)
mulProxy _ _ = Proxy
#if MIN_VERSION_base(4,8,0)
subProxy :: Proxy a -> Proxy b -> Proxy (a - b)
subProxy _ _ = Proxy
#else
subProxy :: Proxy a -> Proxy b -> Proxy c
subProxy _ _ = error "Exp.(-): undefined"
#endif
--  fromInteger = LitT . NumTyLit
#else
addProxy :: Proxy a -> Proxy b -> Proxy c
addProxy _ _ = error "Exp.(+): undefined"
mulProxy :: Proxy a -> Proxy b -> Proxy c
mulProxy _ _ = error "Exp.(*): undefined"
subProxy :: Proxy a -> Proxy b -> Proxy c
subProxy _ _ = error "Exp.(-): undefined"
#endif

#endif
