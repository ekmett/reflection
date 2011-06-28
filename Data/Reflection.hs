{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness -fno-float-in #-}

----------------------------------------------------------------------------
-- |
-- Module     : Data.Reflection
-- Copyright  : 2009-2011 Edward Kmett, 2004 Oleg Kiselyov and Chung-chieh Shan
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (scoped types, MPTCs, rank-n, FFI, kinds)
--
-- Based on the Functional Pearl: Implicit Configurations paper by
-- Oleg Kiselyov and Chung-chieh Shan.
--
-- <http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf>
--
-- Modified to minimize extensions and work with Data.Proxy rather than 
-- explicit scoped type variables and undefined values by Edward Kmett.
--
-------------------------------------------------------------------------------

module Data.Reflection 
    ( 
    -- * Reflect Integrals
      ReifiesNum
    , reflectNum
    , reifyIntegral
    -- * Reflect Lists of Integrals
    , ReifiesNums
    , reifyIntegrals
    -- * Reflect Storables
    , ReifiesStorable
    , reflectStorable
    , reifyStorable
    -- * Reflect Anything
    , Reifies
    , reflect
    , reify
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


class ReifiesNum s where
  reflectNum :: Num a => Proxy s -> a

instance ReifiesNum Zero where
  reflectNum = pure 0

pop :: Proxy (f s) -> Proxy s
pop Proxy = Proxy
{-# INLINE pop #-} 

instance ReifiesNum s => ReifiesNum (Twice s) where
  reflectNum p = 2 * reflectNum (pop p)

instance ReifiesNum s => ReifiesNum (Succ s) where
  reflectNum p = 1 + reflectNum (pop p)

instance ReifiesNum s => ReifiesNum (Pred s) where
  reflectNum p = reflectNum (pop p) - 1

reifyIntegral :: Integral a => a -> (forall s. ReifiesNum s => Proxy s -> w) -> w
reifyIntegral i k = case quotRem i 2 of
    (0, 0) -> zero k 
    (j, 0) -> reifyIntegral j (k . twice)
    (j, 1) -> reifyIntegral j (k . succ . twice)
    (j,-1) -> reifyIntegral j (k . pred . twice)
    _      -> undefined 

twice :: Proxy s -> Proxy (Twice s)
twice _ = Proxy

succ :: Proxy s -> Proxy (Succ s)
succ _ = Proxy 

pred :: Proxy s -> Proxy (Pred s)
pred _ = Proxy

zero :: (Proxy Zero -> a) -> a
zero k = k Proxy

newtype Nil = Nil Nil
newtype Cons s ss = Cons (Cons s ss)
instance Unused Nil where unused Nil{} = ()
instance Unused (Cons s ss) where unused Cons{} = ()

class ReifiesNums ss where
  reflectNums :: Num a => Proxy ss -> [a]

instance ReifiesNums Nil where
  reflectNums = pure []

instance (ReifiesNum s, ReifiesNums ss) => ReifiesNums (Cons s ss) where
  reflectNums p = reflectNum (car p) : reflectNums (cdr p) where
    car :: Proxy (Cons s ss) -> Proxy s
    car _ = Proxy
    cdr :: Proxy (Cons s ss) -> Proxy ss
    cdr _ = Proxy

reifyIntegrals :: Integral a => [a] -> (forall ss. ReifiesNums ss => Proxy ss -> w) -> w
reifyIntegrals [] k = nil k where
    nil :: (Proxy Nil -> a') -> a'
    nil f = f Proxy
reifyIntegrals (i:ii) k = reifyIntegral i (reifyIntegrals ii (cons k)) where
    cons :: (Proxy (Cons s' ss') -> a') -> Proxy ss' -> Proxy s' -> a'
    cons f _ _ = f Proxy
    
newtype Store s a = Store (Store s a)
instance Unused (Store s a) where unused Store{} = ()

class ReifiesStorable s where
    reflectStorable :: Storable a => Proxy (s a) -> a

instance ReifiesNums s => ReifiesStorable (Store s) where
    reflectStorable = r where 
        r = unsafePerformIO $ alloca $ \p -> do 
            pokeArray (castPtr p) (bytes reflectNums r)
            pure <$> peek p 
        bytes :: (Proxy s' -> [CChar]) -> (Proxy (Store s' b) -> b) -> [CChar]
        bytes k _ = k Proxy
    {-# NOINLINE reflectStorable #-}

store :: Proxy s' -> Proxy (Store s' c)
store _ = Proxy

reifyStorable :: Storable a => a -> (forall s. ReifiesStorable s => Proxy (s a) -> w) -> w
reifyStorable a k = reifyIntegrals bytes (k . store)
  where
    bytes :: [CChar]
    bytes = unsafePerformIO $ with a (peekArray (sizeOf a) . castPtr) 
{-# NOINLINE reifyStorable #-}

class Reifies s a | s -> a where 
    reflect :: Proxy s -> a

newtype Stable s a = Stable (s (Stable s a))
instance Unused (Stable s a) where 
    unused Stable{} = ()

instance ReifiesStorable s => Reifies (Stable s a) a where
    reflect = r where
        r = unsafePerformIO $ 
                 pure <$> deRefStablePtr p <* freeStablePtr p
        p = pointer reflectStorable r

        pointer :: (Proxy (s' p) -> p) -> (Proxy (Stable s' a') -> a') -> p
        pointer f _ = f Proxy
    {-# NOINLINE reflect #-}

-- This had to be moved to the top level, due to an apparent bug in the ghc inliner introduced in ghc 7.0.x
reflectBefore :: Reifies s a => (Proxy s -> b) -> Proxy s -> b
reflectBefore f = let b = f Proxy in b `seq` const b
{-# NOINLINE reflectBefore #-}

reify :: a -> (forall s. Reifies s a => Proxy s -> w) -> w
reify a k = unsafePerformIO $ do
        p <- newStablePtr a
        reifyStorable p (reflectBefore (return <$> k) . stable)
    where
        stable :: Proxy (s' (StablePtr a')) -> Proxy (Stable s' a')
        stable _ = Proxy
{-# NOINLINE reify #-}
