{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, UndecidableInstances #-}
import Data.Reflection -- from reflection
import Data.Semigroup  -- from base
import Data.Proxy      -- from tagged

-- | Values in our dynamically-constructed 'Monoid' over 'a'
newtype M a s = M { runM :: a } deriving (Eq,Ord)

-- | A dictionary describing a 'Monoid'
data Monoid_ a = Monoid_ { mappend_ :: a -> a -> a, mempty_ :: a }

instance Reifies s (Monoid_ a) => Semigroup (M a s) where
  a <> b = M $ mappend_ (reflect a) (runM a) (runM b)

instance Reifies s (Monoid_ a) => Monoid (M a s) where
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif
  mempty = a where a = M $ mempty_ (reflect a)

-- Construct a 'Monoid' instance out of a binary operation and unit that you have in scope!
--
-- > ghci> withMonoid (+) 0 $ mempty <> M 2
-- > 2
withMonoid :: (a -> a -> a) -> a -> (forall s. Reifies s (Monoid_ a) => M a s) -> a
withMonoid f z v = reify (Monoid_ f z) (runM . asProxyOf v)

asProxyOf :: f s -> Proxy s -> f s
asProxyOf a _ = a
