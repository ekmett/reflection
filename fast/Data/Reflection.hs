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
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
#define USE_TYPE_LITS 1
#endif
#ifdef MIN_VERSION_template_haskell
# if __GLASGOW_HASKELL__ >= 800
-- TH-subset that works with stage1 & unregisterised GHCs
{-# LANGUAGE TemplateHaskellQuotes #-}
# else
{-# LANGUAGE TemplateHaskell #-}
# endif
#endif

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-float-in #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

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
    , reifyTypeable
    -- * Given
    , Given(..)
    , give
#ifdef MIN_VERSION_template_haskell
    -- * Template Haskell reflection
    , int, nat
#endif
    -- * Useful compile time naturals
    , Z, D, SD, PD

    -- * Reified Monoids
    , ReifiedMonoid(..)
    , ReflectedMonoid(..)
    , reifyMonoid
    , foldMapBy
    , foldBy

    -- * Reified Applicatives
    , ReifiedApplicative(..)
    , ReflectedApplicative(..)
    , reifyApplicative
    , traverseBy
    , sequenceBy
    ) where

import Control.Applicative

#ifdef MIN_VERSION_template_haskell
import Control.Monad
#endif

import Data.Bits

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
#endif

import Data.Semigroup as Sem
import Data.Proxy

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif

import Data.Typeable
import Data.Word
import Foreign.Ptr
import Foreign.StablePtr

#if (__GLASGOW_HASKELL__ >= 707) || (defined(MIN_VERSION_template_haskell) && USE_TYPE_LITS)
import GHC.TypeLits
#endif

#ifdef __HUGS__
import Hugs.IOExts
#endif

#ifdef MIN_VERSION_template_haskell
import Language.Haskell.TH hiding (reify)
#endif

import System.IO.Unsafe

#ifndef __HUGS__
import Unsafe.Coerce
#endif

#ifdef HLINT
{-# ANN module "HLint: ignore Avoid lambda" #-}
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

--------------------------------------------------------------------------------
-- KnownNat
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- KnownSymbol
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Explicit Numeric Reflection
--------------------------------------------------------------------------------

-- | 0
data Z
-- | 2/n/
data D  (n :: *)
-- | 2/n/ + 1
data SD (n :: *)
-- | 2/n/ - 1
data PD (n :: *)

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

--------------------------------------------------------------------------------
-- * Typeable Reflection
--------------------------------------------------------------------------------


class Typeable s => B s where
  reflectByte :: proxy s -> IntPtr

#define BYTES(GO) \
  GO(T0,0) GO(T1,1) GO(T2,2) GO(T3,3) GO(T4,4) GO(T5,5) GO(T6,6) GO(T7,7) GO(T8,8) GO(T9,9) GO(T10,10) GO(T11,11) \
  GO(T12,12) GO(T13,13) GO(T14,14) GO(T15,15) GO(T16,16) GO(T17,17) GO(T18,18) GO(T19,19) GO(T20,20) GO(T21,21) GO(T22,22) \
  GO(T23,23) GO(T24,24) GO(T25,25) GO(T26,26) GO(T27,27) GO(T28,28) GO(T29,29) GO(T30,30) GO(T31,31) GO(T32,32) GO(T33,33) \
  GO(T34,34) GO(T35,35) GO(T36,36) GO(T37,37) GO(T38,38) GO(T39,39) GO(T40,40) GO(T41,41) GO(T42,42) GO(T43,43) GO(T44,44) \
  GO(T45,45) GO(T46,46) GO(T47,47) GO(T48,48) GO(T49,49) GO(T50,50) GO(T51,51) GO(T52,52) GO(T53,53) GO(T54,54) GO(T55,55) \
  GO(T56,56) GO(T57,57) GO(T58,58) GO(T59,59) GO(T60,60) GO(T61,61) GO(T62,62) GO(T63,63) GO(T64,64) GO(T65,65) GO(T66,66) \
  GO(T67,67) GO(T68,68) GO(T69,69) GO(T70,70) GO(T71,71) GO(T72,72) GO(T73,73) GO(T74,74) GO(T75,75) GO(T76,76) GO(T77,77) \
  GO(T78,78) GO(T79,79) GO(T80,80) GO(T81,81) GO(T82,82) GO(T83,83) GO(T84,84) GO(T85,85) GO(T86,86) GO(T87,87) GO(T88,88) \
  GO(T89,89) GO(T90,90) GO(T91,91) GO(T92,92) GO(T93,93) GO(T94,94) GO(T95,95) GO(T96,96) GO(T97,97) GO(T98,98) GO(T99,99) \
  GO(T100,100) GO(T101,101) GO(T102,102) GO(T103,103) GO(T104,104) GO(T105,105) GO(T106,106) GO(T107,107) GO(T108,108) \
  GO(T109,109) GO(T110,110) GO(T111,111) GO(T112,112) GO(T113,113) GO(T114,114) GO(T115,115) GO(T116,116) GO(T117,117) \
  GO(T118,118) GO(T119,119) GO(T120,120) GO(T121,121) GO(T122,122) GO(T123,123) GO(T124,124) GO(T125,125) GO(T126,126) \
  GO(T127,127) GO(T128,128) GO(T129,129) GO(T130,130) GO(T131,131) GO(T132,132) GO(T133,133) GO(T134,134) GO(T135,135) \
  GO(T136,136) GO(T137,137) GO(T138,138) GO(T139,139) GO(T140,140) GO(T141,141) GO(T142,142) GO(T143,143) GO(T144,144) \
  GO(T145,145) GO(T146,146) GO(T147,147) GO(T148,148) GO(T149,149) GO(T150,150) GO(T151,151) GO(T152,152) GO(T153,153) \
  GO(T154,154) GO(T155,155) GO(T156,156) GO(T157,157) GO(T158,158) GO(T159,159) GO(T160,160) GO(T161,161) GO(T162,162) \
  GO(T163,163) GO(T164,164) GO(T165,165) GO(T166,166) GO(T167,167) GO(T168,168) GO(T169,169) GO(T170,170) GO(T171,171) \
  GO(T172,172) GO(T173,173) GO(T174,174) GO(T175,175) GO(T176,176) GO(T177,177) GO(T178,178) GO(T179,179) GO(T180,180) \
  GO(T181,181) GO(T182,182) GO(T183,183) GO(T184,184) GO(T185,185) GO(T186,186) GO(T187,187) GO(T188,188) GO(T189,189) \
  GO(T190,190) GO(T191,191) GO(T192,192) GO(T193,193) GO(T194,194) GO(T195,195) GO(T196,196) GO(T197,197) GO(T198,198) \
  GO(T199,199) GO(T200,200) GO(T201,201) GO(T202,202) GO(T203,203) GO(T204,204) GO(T205,205) GO(T206,206) GO(T207,207) \
  GO(T208,208) GO(T209,209) GO(T210,210) GO(T211,211) GO(T212,212) GO(T213,213) GO(T214,214) GO(T215,215) GO(T216,216) \
  GO(T217,217) GO(T218,218) GO(T219,219) GO(T220,220) GO(T221,221) GO(T222,222) GO(T223,223) GO(T224,224) GO(T225,225) \
  GO(T226,226) GO(T227,227) GO(T228,228) GO(T229,229) GO(T230,230) GO(T231,231) GO(T232,232) GO(T233,233) GO(T234,234) \
  GO(T235,235) GO(T236,236) GO(T237,237) GO(T238,238) GO(T239,239) GO(T240,240) GO(T241,241) GO(T242,242) GO(T243,243) \
  GO(T244,244) GO(T245,245) GO(T246,246) GO(T247,247) GO(T248,248) GO(T249,249) GO(T250,250) GO(T251,251) GO(T252,252) \
  GO(T253,253) GO(T254,254) GO(T255,255)

#define GO(Tn,n) \
  newtype Tn = Tn Tn deriving Typeable; \
  instance B Tn where { \
    reflectByte _ = n \
  };
BYTES(GO)
#undef GO

impossible :: a
impossible = error "Data.Reflection.reifyByte: impossible"

reifyByte :: Word8 -> (forall (s :: *). B s => Proxy s -> r) -> r
reifyByte w k = case w of {
#define GO(Tn,n) n -> k (Proxy :: Proxy Tn);
BYTES(GO)
#undef GO
_ -> impossible
}

newtype W (b0 :: *) (b1 :: *) (b2 :: *) (b3 :: *) = W (W b0 b1 b2 b3) deriving Typeable
newtype Stable (w0 :: *) (w1 :: *) (a :: *) = Stable (Stable w0 w1 a) deriving Typeable

stable :: p b0 -> p b1 -> p b2 -> p b3 -> p b4 -> p b5 -> p b6 -> p b7
       -> Proxy (Stable (W b0 b1 b2 b3) (W b4 b5 b6 b7) a)
stable _ _ _ _ _ _ _ _ = Proxy
{-# INLINE stable #-}

stablePtrToIntPtr :: StablePtr a -> IntPtr
stablePtrToIntPtr = ptrToIntPtr . castStablePtrToPtr
{-# INLINE stablePtrToIntPtr #-}

intPtrToStablePtr :: IntPtr -> StablePtr a
intPtrToStablePtr = castPtrToStablePtr . intPtrToPtr
{-# INLINE intPtrToStablePtr #-}

byte0 :: p (Stable (W b0 b1 b2 b3) w1 a) -> Proxy b0
byte0 _ = Proxy

byte1 :: p (Stable (W b0 b1 b2 b3) w1 a) -> Proxy b1
byte1 _ = Proxy

byte2 :: p (Stable (W b0 b1 b2 b3) w1 a) -> Proxy b2
byte2 _ = Proxy

byte3 :: p (Stable (W b0 b1 b2 b3) w1 a) -> Proxy b3
byte3 _ = Proxy

byte4 :: p (Stable w0 (W b4 b5 b6 b7) a) -> Proxy b4
byte4 _ = Proxy

byte5 :: p (Stable w0 (W b4 b5 b6 b7) a) -> Proxy b5
byte5 _ = Proxy

byte6 :: p (Stable w0 (W b4 b5 b6 b7) a) -> Proxy b6
byte6 _ = Proxy

byte7 :: p (Stable w0 (W b4 b5 b6 b7) a) -> Proxy b7
byte7 _ = Proxy

argument :: (p s -> r) -> Proxy s
argument _ = Proxy

instance (B b0, B b1, B b2, B b3, B b4, B b5, B b6, B b7, w0 ~ W b0 b1 b2 b3, w1 ~ W b4 b5 b6 b7)
    => Reifies (Stable w0 w1 a) a where
  reflect = r where
      r = unsafePerformIO $ const <$> deRefStablePtr p <* freeStablePtr p
      s = argument r
      p = intPtrToStablePtr $
        reflectByte (byte0 s) .|.
        (reflectByte (byte1 s) `shiftL` 8) .|.
        (reflectByte (byte2 s) `shiftL` 16) .|.
        (reflectByte (byte3 s) `shiftL` 24) .|.
        (reflectByte (byte4 s) `shiftL` 32) .|.
        (reflectByte (byte5 s) `shiftL` 40) .|.
        (reflectByte (byte6 s) `shiftL` 48) .|.
        (reflectByte (byte7 s) `shiftL` 56)
  {-# NOINLINE reflect #-}

-- This had to be moved to the top level, due to an apparent bug in
-- the ghc inliner introduced in ghc 7.0.x
reflectBefore :: forall (proxy :: * -> *) s b. (Proxy s -> b) -> proxy s -> b
reflectBefore f = const $! f Proxy
{-# NOINLINE reflectBefore #-}

-- | Reify a value at the type level in a 'Typeable'-compatible fashion, to be recovered with 'reflect'.
--
-- This can be necessary to work around the changes to @Data.Typeable@ in GHC HEAD.
reifyTypeable :: Typeable a => a -> (forall (s :: *). (Typeable s, Reifies s a) => Proxy s -> r) -> r
#if MIN_VERSION_base(4,4,0)
reifyTypeable a k = unsafeDupablePerformIO $ do
#else
reifyTypeable a k = unsafePerformIO $ do
#endif
  p <- newStablePtr a
  let n = stablePtrToIntPtr p
  reifyByte (fromIntegral n) (\s0 ->
    reifyByte (fromIntegral (n `shiftR` 8)) (\s1 ->
      reifyByte (fromIntegral (n `shiftR` 16)) (\s2 ->
        reifyByte (fromIntegral (n `shiftR` 24)) (\s3 ->
          reifyByte (fromIntegral (n `shiftR` 32)) (\s4 ->
            reifyByte (fromIntegral (n `shiftR` 40)) (\s5 ->
              reifyByte (fromIntegral (n `shiftR` 48)) (\s6 ->
                reifyByte (fromIntegral (n `shiftR` 56)) (\s7 ->
                  reflectBefore (fmap return k) $
                    stable s0 s1 s2 s3 s4 s5 s6 s7))))))))


data ReifiedMonoid a = ReifiedMonoid { reifiedMappend :: a -> a -> a, reifiedMempty :: a }

instance Reifies s (ReifiedMonoid a) => Sem.Semigroup (ReflectedMonoid a s) where
  ReflectedMonoid x <> ReflectedMonoid y = reflectResult (\m -> ReflectedMonoid (reifiedMappend m x y))

instance Reifies s (ReifiedMonoid a) => Monoid (ReflectedMonoid a s) where
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif
  mempty = reflectResult (\m -> ReflectedMonoid (reifiedMempty  m    ))

reflectResult :: forall f s a. Reifies s a => (a -> f s) -> f s
reflectResult f = f (reflect (Proxy :: Proxy s))

newtype ReflectedMonoid a s = ReflectedMonoid a

unreflectedMonoid :: ReflectedMonoid a s -> proxy s -> a
unreflectedMonoid (ReflectedMonoid a) _ = a

reifyMonoid :: (a -> a -> a) -> a -> (forall (s :: *). Reifies s (ReifiedMonoid a) => t -> ReflectedMonoid a s) -> t -> a
reifyMonoid f z m xs = reify (ReifiedMonoid f z) (unreflectedMonoid (m xs))

-- | Fold a value using its 'Foldable' instance using
-- explicitly provided 'Monoid' operations. This is like 'fold'
-- where the 'Monoid' instance can be manually specified.
--
-- @
-- 'foldBy' 'mappend' 'mempty' ≡ 'fold'
-- @
--
-- >>> foldBy (++) [] ["hello","world"]
-- "helloworld"
foldBy :: Foldable t => (a -> a -> a) -> a -> t a -> a
foldBy f z = reifyMonoid f z (foldMap ReflectedMonoid)

-- | Fold a value using its 'Foldable' instance using
-- explicitly provided 'Monoid' operations. This is like 'foldMap'
-- where the 'Monoid' instance can be manually specified.
--
-- @
-- 'foldMapBy' 'mappend' 'mempty' ≡ 'foldMap'
-- @
--
-- >>> foldMapBy (+) 0 length ["hello","world"]
-- 10
foldMapBy :: Foldable t => (r -> r -> r) -> r -> (a -> r) -> t a -> r
foldMapBy f z g = reifyMonoid f z (foldMap (ReflectedMonoid #. g))

data ReifiedApplicative f = ReifiedApplicative { reifiedPure :: forall a. a -> f a, reifiedAp :: forall a b. f (a -> b) -> f a -> f b }

newtype ReflectedApplicative f s a = ReflectedApplicative (f a)

instance Reifies s (ReifiedApplicative f) => Functor (ReflectedApplicative f s) where
  fmap = liftA

instance Reifies s (ReifiedApplicative f) => Applicative (ReflectedApplicative f s) where
  pure a = reflectResult1 (\m -> ReflectedApplicative (reifiedPure m a))
  ReflectedApplicative x <*> ReflectedApplicative y = reflectResult1 (\m -> ReflectedApplicative (reifiedAp m x y))

reflectResult1 :: forall f s a b. Reifies s a => (a -> f s b) -> f s b
reflectResult1 f = f (reflect (Proxy :: Proxy s))

unreflectedApplicative :: ReflectedApplicative f s a -> proxy s -> f a
unreflectedApplicative (ReflectedApplicative a) _ = a

reifyApplicative :: (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> (forall (s :: *). Reifies s (ReifiedApplicative f) => t -> ReflectedApplicative f s a) -> t -> f a
reifyApplicative f g m xs = reify (ReifiedApplicative f g) (unreflectedApplicative (m xs))

-- | Traverse a container using its 'Traversable' instance using
-- explicitly provided 'Applicative' operations. This is like 'traverse'
-- where the 'Applicative' instance can be manually specified.
traverseBy :: Traversable t => (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> (a -> f b) -> t a -> f (t b)
traverseBy pur app f = reifyApplicative pur app (traverse (ReflectedApplicative #. f))

-- | Sequence a container using its 'Traversable' instance using
-- explicitly provided 'Applicative' operations. This is like 'sequence'
-- where the 'Applicative' instance can be manually specified.
sequenceBy :: Traversable t => (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> t (f a) -> f (t a)
sequenceBy pur app = reifyApplicative pur app (traverse ReflectedApplicative)

(#.) :: (b -> c) -> (a -> b) -> a -> c
(#.) _ = unsafeCoerce
