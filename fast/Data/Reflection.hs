{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Reflection
-- Copyright  : 2009-2012 Edward Kmett,
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
-- <http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf>
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
-------------------------------------------------------------------------------
module Data.Reflection
    (
      Reifies(..)
    , reify
    ) where

import Data.Proxy

#ifdef __HUGS__
import Hugs.IOExts
#else
import Unsafe.Coerce
#endif

class Reifies s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its
  -- reified type.
  reflect :: proxy s -> a

newtype Magic a r = Magic (forall s. Reifies s a => Proxy s -> r)

-- | Reify a value at the type level, to be recovered with 'reflect'.
reify :: a -> (forall s. Reifies s a => Proxy s -> r) -> r
reify a k = (unsafeCoerce (Magic k) $! const a) Proxy
