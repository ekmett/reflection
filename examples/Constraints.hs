{-# LANGUAGE Rank2Types, TypeFamilies, TypeOperators, ConstraintKinds, PolyKinds, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
import Control.Newtype        -- from newtype
import Data.Constraint        -- from constraints
import Data.Constraint.Unsafe -- from constraints
import Data.Monoid            -- from base
import Data.Proxy             -- from tagged
import Data.Reflection        -- from reflection

-- | Values in our dynamically constructed monoid over 'a'
newtype Lift (p :: * -> Constraint) (a :: *) (s :: *) = Lift { lower :: a }

class ReifiableConstraint p where
  data Def (p :: * -> Constraint) (a :: *)
  reifiedIns :: Reifies s (Def p a) :- p (Lift p a s)

instance Newtype (Lift p a s) a where
  pack = Lift
  unpack = lower

-- > ghci> with (Monoid (+) 0) $ mempty <> Lift 2
-- > 2
with :: Def p a -> (forall s. Reifies s (Def p a) => Lift p a s) -> a
with d v = reify d $ lower . asProxyOf v

reifyInstance :: Def p a -> (forall s. Reifies s (Def p a) => Proxy s -> r) -> r
reifyInstance = reify

asProxyOf :: f s -> Proxy s -> f s
asProxyOf a _ = a

-- > using (Monoid (+) 0) $ mappend mempty 12
-- > 12
using :: forall p a. ReifiableConstraint p => Def p a -> (p a => a) -> a
using d m = reify d $ \(_ :: Proxy s) -> m \\ trans (unsafeCoerceConstraint :: (p (Lift p a s) :- p a)) reifiedIns

usingT :: forall p f a. ReifiableConstraint p => Def p a -> (p a => f a) -> f a
usingT d m = reify d $ \(_ :: Proxy s) -> m \\ trans (unsafeCoerceConstraint :: (p (Lift p a s) :- p a)) reifiedIns

instance ReifiableConstraint Monoid where
  data Def Monoid a = Monoid { mappend_ :: a -> a -> a, mempty_ :: a }
  reifiedIns = Sub Dict

instance Reifies s (Def Monoid a) => Monoid (Lift Monoid a s) where
  mappend a b        = Lift $ mappend_ (reflect a) (lower a) (lower b)
  mempty = a where a = Lift $ mempty_ (reflect a)

instance ReifiableConstraint Eq where
  data Def Eq a = Eq { eq_ :: a -> a -> Bool }
  reifiedIns = Sub Dict

instance Reifies s (Def Eq a) => Eq (Lift Eq a s) where
  a == b = eq_ (reflect a) (lower a) (lower b)
