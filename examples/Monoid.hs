{-# LANGUAGE Rank2Types, TypeFamilies #-}
import Data.Reflection
import Data.Monoid
import Data.Proxy

-- | Values in our dynamically constructed monoid over 'a'
newtype M a s = M { runM :: a } deriving (Eq,Ord)

-- | A dictionary describing the contents of a monoid
data Monoid_ a = Monoid_ { mappend_ :: a -> a -> a, mempty_ :: a }

instance (Reified s, Reflected s ~ Monoid_ a) => Monoid (M a s) where
  mappend a b        = M $ mappend_ (reflect a) (runM a) (runM b)
  mempty = a where a = M $ mempty_ (reflect a)

reifyMonoid :: (a -> a -> a) -> a -> (forall s. (Reified s, Reflected s ~ Monoid_ a) => Proxy s -> r) -> r
reifyMonoid f z = reify (Monoid_ f z)

-- > ghci> withMonoid (+) 0 $ mempty <> M 2
-- > 2
withMonoid :: (a -> a -> a) -> a -> (forall s. (Reified s, Reflected s ~ Monoid_ a) => M a s) -> a
withMonoid f z v = reifyMonoid f z (\p -> runM (asProxyOf v p))

asProxyOf :: M a s -> Proxy s -> M a s
asProxyOf a _ = a
