{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness -fno-float-in -fno-warn-unused-binds #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Reflection
-- Copyright  : 2009-2012 Edward Kmett, 2004 Oleg Kiselyov and Chung-chieh Shan
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (rank-2 types)
--
-- Based on the Functional Pearl: Implicit Configurations paper by
-- Oleg Kiselyov and Chung-chieh Shan.
--
-- <http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf>
--
-- Modified to minimize extensions and work with Data.Proxy rather than
-- explicit scoped type variables and undefined values by Edward Kmett.
-------------------------------------------------------------------------------

module Data.Reflection
    (
    -- * Reifying any term at the type level
      Reified(..)
    , reify
    , reflectT
    -- * Reifying integral values at the type level
    , ReifiedNum(..)
    , reifyIntegral
    ) where

import Foreign.Ptr
import Foreign.StablePtr
import System.IO.Unsafe
import Control.Applicative
import Prelude hiding (succ, pred)
import Data.Proxy

newtype Zero = Zero Zero deriving (Show)
newtype Twice s = Twice (Twice s) deriving (Show)
newtype SuccTwice s = SuccTwice (SuccTwice s) deriving (Show)
newtype PredTwice s = PredTwice (PredTwice s) deriving (Show)

class ReifiedNum s where
  reflectNum :: Num a => proxy s -> a

instance ReifiedNum Zero where
  reflectNum = pure 0

pop :: proxy (f s) -> Proxy s
pop _ = Proxy
{-# INLINE pop #-}

instance ReifiedNum s => ReifiedNum (Twice s) where
  reflectNum p = 2 * reflectNum (pop p)

instance ReifiedNum s => ReifiedNum (SuccTwice s) where
  reflectNum p = 2 * reflectNum (pop p) + 1

instance ReifiedNum s => ReifiedNum (PredTwice s) where
  reflectNum p = 2 * reflectNum (pop p) - 1

reifyIntegral :: Integral a => a -> (forall s. ReifiedNum s => Proxy s -> w) -> w
reifyIntegral i k = case quotRem i 2 of
    (0, 0) -> zero k
    (j, 0) -> reifyIntegral j (k . twice)
    (j, 1) -> reifyIntegral j (k . succTwice)
    (j,-1) -> reifyIntegral j (k . predTwice)
    _      -> undefined

twice :: proxy s -> Proxy (Twice s)
twice _ = Proxy

succTwice :: proxy s -> Proxy (SuccTwice s)
succTwice _ = Proxy

predTwice :: proxy s -> Proxy (PredTwice s)
predTwice _ = Proxy

zero :: (Proxy Zero -> a) -> a
zero k = k Proxy

newtype Stable s a = Stable (Stable s a)

stablePtrToIntPtr :: StablePtr a -> IntPtr
stablePtrToIntPtr = ptrToIntPtr . castStablePtrToPtr

intPtrToStablePtr :: IntPtr -> StablePtr a
intPtrToStablePtr = castPtrToStablePtr . intPtrToPtr

class Reified s where
  reflect :: p (s a) -> a

unstable :: (p (Stable s a) -> a) -> Proxy s
unstable _ = Proxy

instance ReifiedNum s => Reified (Stable s) where
  reflect = r where
      r = unsafePerformIO $ pure <$> deRefStablePtr p <* freeStablePtr p
      p = intPtrToStablePtr $ reflectNum $ unstable r
  {-# NOINLINE reflect #-}

reflectT :: Reified s => t s a -> a
reflectT p = reflect (t p) where
  t :: p x y -> Proxy (x y)
  t _ = Proxy

-- This had to be moved to the top level, due to an apparent bug in the ghc inliner introduced in ghc 7.0.x
reflectBefore :: Reified s => (Proxy (s a) -> b) -> proxy (s a) -> b
reflectBefore f = let b = f Proxy in b `seq` const b
{-# NOINLINE reflectBefore #-}

reify :: a -> (forall s. Reified s => Proxy (s a) -> w) -> w
reify a k = unsafePerformIO $ do
    p <- newStablePtr a
    reifyIntegral (stablePtrToIntPtr p) (reflectBefore (fmap return k) . stable)
  where
    stable :: p s -> Proxy (Stable s a)
    stable _ = Proxy
{-# NOINLINE reify #-}
