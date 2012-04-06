{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness -fno-float-in #-}
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
      Reified
    , reflect, reflectT
    , reify
    ) where

import Data.Reflection.Internal
