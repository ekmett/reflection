{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeOperators #-}

----------------------------------------------------------------------------
-- |
-- Module     : Data.Reflection
-- Copyright  : 2009 Edward Kmett, 2004 Oleg Kiselyov and Chung-chieh Shan
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
-- Modified to minimize extensions and work with Data.Tagged rather than 
-- explicit scoped type variables by Edward Kmett.
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
import Data.Tagged

class Unused t where unused :: t -> ()

type Retag f g = forall b. Tagged g b -> Tagged f b

newtype Zero = Zero Zero deriving (Show)
newtype Twice s = Twice (Twice s) deriving (Show)
newtype Succ s = Succ (Succ s) deriving (Show)
newtype Pred s = Pred (Pred s) deriving (Show)
instance Unused Zero where unused Zero{} = ()
instance Unused (Twice s) where unused Twice{} = ()
instance Unused (Succ s) where unused Succ{} = ()
instance Unused (Pred s) where unused Pred{} = ()

pop :: Retag (f s) s
pop = retag
{-# INLINE pop #-} 

class ReifiesNum s where
    reflectNum :: Num a => Tagged s a

instance ReifiesNum Zero where
    reflectNum = pure 0

instance ReifiesNum s => ReifiesNum (Twice s) where
    reflectNum = (2*) <$> pop reflectNum 

instance ReifiesNum s => ReifiesNum (Succ s) where
    reflectNum = (1+) <$> pop reflectNum

instance ReifiesNum s => ReifiesNum (Pred s) where
    reflectNum = subtract 1 <$> pop reflectNum

reifyIntegral :: Integral a => a -> (forall s. ReifiesNum s => Tagged s w) -> w
reifyIntegral i k = case quotRem i 2 of
    (0, 0) -> zero k 
    (j, 0) -> reifyIntegral j (twice k)
    (j, 1) -> reifyIntegral j (twice (succ k))
    (j,-1) -> reifyIntegral j (twice (pred k))
    _      -> undefined 
  where
    twice :: Retag s (Twice s)
    twice = retag
    succ :: Retag s (Succ s)
    succ = retag
    pred :: Retag s (Pred s)
    pred = retag
    zero :: Tagged Zero a -> a
    zero = unTagged

newtype Nil = Nil Nil
newtype Cons s ss = Cons (Cons s ss)
instance Unused Nil where unused Nil{} = ()
instance Unused (Cons s ss) where unused Cons{} = ()

class ReifiesNums ss where
    reflectNums :: Num a => Tagged ss [a]

instance ReifiesNums Nil where
    reflectNums = pure []

instance (ReifiesNum s, ReifiesNums ss) => ReifiesNums (Cons s ss) where
    reflectNums = (:) <$> car reflectNum <*> cdr reflectNums where
        car :: Retag (Cons s ss) s
        car = retag
        cdr :: Retag (Cons s ss) ss
        cdr = retag

reifyIntegrals :: Integral a => [a] -> (forall ss. ReifiesNums ss => Tagged ss w) -> w
reifyIntegrals [] k = nil k where
    nil :: Tagged Nil a' -> a'
    nil = unTagged 
reifyIntegrals (i:ii) k = reifyIntegral i (reifyIntegrals ii (cons k)) where
    cons :: Tagged (Cons s' ss') a' -> Tagged ss' (Tagged s' a')
    cons = pure . retag
    
newtype Store s a = Store (Store s a)
instance Unused (Store s a) where unused Store{} = ()

class ReifiesStorable s where
    reflectStorable :: Storable a => Tagged (s a) a

instance ReifiesNums s => ReifiesStorable (Store s) where
    reflectStorable = r where 
        r = unsafePerformIO $ alloca $ \p -> do 
            pokeArray (castPtr p) (bytes reflectNums r)
            pure <$> peek p 
        bytes :: Tagged s' [CChar] -> Tagged (Store s' b) b -> [CChar]
        bytes (Tagged a) _ = a
    {-# NOINLINE reflectStorable #-}

store :: Retag s' (Store s' c)
store = retag

reifyStorable :: Storable a => a -> (forall s. ReifiesStorable s => Tagged (s a) w) -> w
reifyStorable a k = reifyIntegrals bytes (store k)
  where
    bytes :: [CChar]
    bytes = unsafePerformIO $ with a (peekArray (sizeOf a) . castPtr) 
{-# NOINLINE reifyStorable #-}

class Reifies s a | s -> a where 
    reflect :: Tagged s a

newtype Stable s a = Stable (s (Stable s a))
instance Unused (Stable s a) where unused Stable{} = ()

instance ReifiesStorable s => Reifies (Stable s a) a where
    reflect = r where
        r = unsafePerformIO $ do
            pure <$> deRefStablePtr p <* freeStablePtr p

        p = pointer reflectStorable r

        pointer :: Tagged (s' p) p -> Tagged (Stable s' a') a' -> p
        pointer (Tagged a) _ = a
    {-# NOINLINE reflect #-}

reify :: a -> (forall s. Reifies s a => Tagged s w) -> w
reify a k = unsafePerformIO $ do
        p <- newStablePtr a
        reifyStorable p (stable (reflect `before` (return <$> k)))
    where
        stable :: Retag (s' (StablePtr a')) (Stable s' a')
        stable = retag
        before :: Tagged (s' a') a' -> Tagged (s' a') r -> Tagged (s' a') r
        before = seq
{-# NOINLINE reify #-}

