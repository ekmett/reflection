{-# LANGUAGE Rank2Types, KindSignatures #-}
import Data.Reflection
import Data.Monoid
import Data.Proxy

newtype M (s :: * -> *) a = M { runM :: a }

data Monoid_ a = Monoid_ { mappend_ :: a -> a -> a, mempty_ :: a }

instance Reified s => Monoid (M s a) where
  mappend a b = M $ mappend_ (reflectM a) (runM a) (runM b)
  mempty = r where r = M $ mempty_ $ reflectM r

monoidProxy :: M s a -> Proxy (s (Monoid_ a))
monoidProxy _ = Proxy

reflectM :: Reified s => M s a -> Monoid_ a
reflectM m = reflect (monoidProxy m) where

reifyMonoid :: (a -> a -> a) -> a -> (forall s. Reified s => Proxy (s (Monoid_ a)) -> r) -> r
reifyMonoid f z = reify (Monoid_ f z)

-- > ghci> withMonoid (+) 0 (mempty `mappend` M 2)
-- > 2
withMonoid :: (a -> a -> a) -> a -> (forall s. Reified s => M s a) -> a
withMonoid f z v = reifyMonoid f z (\p -> runM (v `asProxyOf` p)) where
  asProxyOf :: M s x -> Proxy (s (Monoid_ x)) -> M s x
  asProxyOf a _ = a
