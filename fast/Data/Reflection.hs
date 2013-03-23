{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Reflection
-- Copyright  : 2009-2013 Edward Kmett,
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
--
-- In addition, a simpler API is offered for working with singleton
-- values such as a system configuration, etc.
-------------------------------------------------------------------------------
module Data.Reflection
    (
    -- * Reflection
      Reifies(..)
    , reify
    -- * Given
    , Given(..)
    , give
    ) where

import Data.Proxy

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

newtype Magic a r = Magic (forall s. Reifies s a => Proxy s -> r)

-- | Reify a value at the type level, to be recovered with 'reflect'.
reify :: forall a r. a -> (forall s. Reifies s a => Proxy s -> r) -> r
reify a k = unsafeCoerce (Magic k :: Magic a r) (const a) Proxy
{-# INLINE reify #-}

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
-- You should only 'give' a single value for each type. If multiple instances
-- are in scope, then the behavior is implementation defined.
give :: forall a r. a -> (Given a => r) -> r
give a k = unsafeCoerce (Gift k :: Gift a r) a
{-# INLINE give #-}
