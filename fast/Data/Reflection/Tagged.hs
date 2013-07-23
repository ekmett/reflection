{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Reflection.Tagged
-- Copyright  : 2009-2013 Edward Kmett,
--              2012 Elliott Hird,
--              2004 Oleg Kiselyov and Chung-chieh Shan
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Combinators built on top of 'Data.Reflection.Given'
-- that allows multiple reflected values of one type by
-- type-level tagging.
--
-- >>> :set -XConstraintKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XRankNTypes
-- >>> :set -XTypeOperators
-- >>> data Mod = Mod
-- >>> :{
-- >>> let { plus :: (Mod ::: Int) => Int -> Int -> Int
-- >>>     ; plus x y = (x + y) `mod` the Mod }
-- >>> :}
-- >>> Mod `being` (10 :: Int) $ plus 6 7
-- 3
-- >>> Mod `being` (2 :: Int) $ plus 6 7
-- 1
--
-- https://github.com/ekmett/reflection/blob/master/examples/Monoid.hs
----------------------------------------------------------------------------

module Data.Reflection.Tagged where

import Data.Reflection (Given(..), give)
import Data.Tagged (Tagged(..))

infix 1 `being`

-- | Set a value of type @a@ for a type-level tag @tag@ .

being :: forall tag a r. tag -> a -> (Given (Tagged tag a) => r) -> r
being _ val = give (Tagged val)


-- | Recover a value from the @tag@ .

the :: forall tag a. Given (Tagged tag a) => tag -> a
the _ = unTagged (given :: Tagged tag a)


-- | Synonym for class constraint saying that the @tag@ provides a
--   value of type @a@ .

type tag ::: a = Given (Tagged tag a)
