{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
import Data.Reflection -- from reflection
import Data.Monoid     -- from base
import Data.Proxy      -- from tagged

-- | Values in our dynamically constructed monoid over 'a'
newtype M a s = M { runM :: a } deriving (Eq,Ord)

-- | A dictionary describing the contents of a monoid
data Monoid_ a = Monoid_ { mappend_ :: a -> a -> a, mempty_ :: a }

instance Reifies s (Monoid_ a) => Monoid (M a s) where
  mappend a b        = M $ mappend_ (reflect a) (runM a) (runM b)
  mempty = a where a = M $ mempty_ (reflect a)

-- > ghci> withMonoid (+) 0 $ mempty <> M 2
-- > 2
withMonoid :: (a -> a -> a) -> a -> (forall s. Reifies s (Monoid_ a) => M a s) -> a
withMonoid f z v = reify (Monoid_ f z) (runM . asProxyOf v)

asProxyOf :: f s -> Proxy s -> f s
asProxyOf a _ = a
