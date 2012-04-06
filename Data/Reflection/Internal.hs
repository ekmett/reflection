{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness -fno-float-in #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Reflection.Internal
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

module Data.Reflection.Internal
    (
    -- * Reifying any term at the type level
      Reified(..)
    , reify
    , reflectT
    -- * Special cases
    -- ** Reifying integral values as types
    , ReifiedNum(..)
    , reifyIntegral
    -- ** Reifying lists of integrals values as types
    , ReifiedNums(..)
    , reifyIntegrals
    -- ** Reifying storables values as types
    , ReifiedStorable(..)
    , reifyStorable
    ) where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import System.IO.Unsafe
import Control.Applicative
import Prelude hiding (succ, pred)
import Data.Proxy

newtype Zero = Zero Zero deriving (Show)
newtype Twice s = Twice (Twice s) deriving (Show)
newtype Succ s = Succ (Succ s) deriving (Show)
newtype Pred s = Pred (Pred s) deriving (Show)

-- silence compiler warnings about unused constructors
class Unused t where
  unused :: t -> ()
instance Unused Zero where
  unused Zero{} = ()
instance Unused (Twice s) where
  unused Twice{} = ()
instance Unused (Succ s) where
  unused Succ{} = ()
instance Unused (Pred s) where
  unused Pred{} = ()

class ReifiedNum s where
  reflectNum :: Num a => proxy s -> a

instance ReifiedNum Zero where
  reflectNum = pure 0

pop :: proxy (f s) -> Proxy s
pop _ = Proxy
{-# INLINE pop #-}

instance ReifiedNum s => ReifiedNum (Twice s) where
  reflectNum p = 2 * reflectNum (pop p)

instance ReifiedNum s => ReifiedNum (Succ s) where
  reflectNum p = 1 + reflectNum (pop p)

instance ReifiedNum s => ReifiedNum (Pred s) where
  reflectNum p = reflectNum (pop p) - 1

reifyIntegral :: Integral a => a -> (forall s. ReifiedNum s => Proxy s -> w) -> w
reifyIntegral i k = case quotRem i 2 of
    (0, 0) -> zero k
    (j, 0) -> reifyIntegral j (k . twice)
    (j, 1) -> reifyIntegral j (k . succ . twice)
    (j,-1) -> reifyIntegral j (k . pred . twice)
    _      -> undefined

twice :: proxy s -> Proxy (Twice s)
twice _ = Proxy

succ :: proxy s -> Proxy (Succ s)
succ _ = Proxy

pred :: proxy s -> Proxy (Pred s)
pred _ = Proxy

zero :: (Proxy Zero -> a) -> a
zero k = k Proxy

newtype Nil = Nil Nil
newtype Cons s ss = Cons (Cons s ss)
instance Unused Nil where unused Nil{} = ()
instance Unused (Cons s ss) where unused Cons{} = ()

class ReifiedNums ss where
  reflectNums :: Num a => proxy ss -> [a]

instance ReifiedNums Nil where
  reflectNums = pure []

instance (ReifiedNum s, ReifiedNums ss) => ReifiedNums (Cons s ss) where
  reflectNums p = reflectNum (car p) : reflectNums (cdr p) where
    car :: proxy (Cons s ss) -> Proxy s
    car _ = Proxy
    cdr :: proxy (Cons s ss) -> Proxy ss
    cdr _ = Proxy

reifyIntegrals :: Integral a => [a] -> (forall ss. ReifiedNums ss => Proxy ss -> w) -> w
reifyIntegrals [] k = nil k where
    nil :: (Proxy Nil -> a') -> a'
    nil f = f Proxy
reifyIntegrals (i:ii) k = reifyIntegral i (reifyIntegrals ii (cons k)) where
    cons :: (Proxy (Cons s' ss') -> a') -> proxy ss' -> proxy s' -> a'
    cons f _ _ = f Proxy

newtype Store s a = Store (Store s a)
instance Unused (Store s a) where unused Store{} = ()

class ReifiedStorable s where
    reflectStorable :: Storable a => proxy (s a) -> a

instance ReifiedNums s => ReifiedStorable (Store s) where
    reflectStorable = r where
        r = unsafePerformIO $ alloca $ \p -> do
            pokeArray (castPtr p) (bytes reflectNums r)
            pure <$> peek p
        bytes :: (Proxy s' -> [CChar]) -> (proxy (Store s' b) -> b) -> [CChar]
        bytes k _ = k Proxy
    {-# NOINLINE reflectStorable #-}

store :: proxy s' -> Proxy (Store s' c)
store _ = Proxy

reifyStorable :: Storable a => a -> (forall s. ReifiedStorable s => Proxy (s a) -> w) -> w
reifyStorable a k = reifyIntegrals bytes (k . store)
  where
    bytes :: [CChar]
    bytes = unsafePerformIO $ with a (peekArray (sizeOf a) . castPtr)
{-# NOINLINE reifyStorable #-}

class Reified s where
    reflect :: proxy (s a) -> a

newtype Stable s a = Stable (s (Stable s a))
instance Unused (Stable s a) where
    unused Stable{} = ()

instance ReifiedStorable s => Reified (Stable s) where
    reflect = r where
        r = unsafePerformIO $ pure <$> deRefStablePtr p <* freeStablePtr p
        p = pointer reflectStorable r

        pointer :: (Proxy (s' p) -> p) -> (proxy (Stable s' a') -> a') -> p
        pointer f _ = f Proxy
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
        reifyStorable p (reflectBefore (return <$> k) . stable)
    where
        stable :: proxy (s' (StablePtr a')) -> Proxy (Stable s' a')
        stable _ = Proxy
{-# NOINLINE reify #-}
