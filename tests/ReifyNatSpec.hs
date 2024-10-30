{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReifyNatSpec where

import Data.Reflection
import Test.Hspec.QuickCheck
import Test.QuickCheck (NonNegative(..))

#if MIN_VERSION_base(4,10,0)
import GHC.TypeNats (natVal)
import Numeric.Natural (Natural)
#endif

#if __GLASGOW_HASKELL__ != 900
import Control.Exception (ArithException(..), evaluate)
import Test.QuickCheck (Negative(..))
#endif

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "reifyNat" $ do
    prop "reify positive Integers and reflect them back" $
      \(NonNegative (i :: Integer)) -> reifyNat i $ \p -> reflect p `shouldBe` i
#if __GLASGOW_HASKELL__ != 900
    -- Inexplicably, this test fails on GHC 9.0 with hspec-2.8.4 or later.
    -- Moreover, I suspect that undefined behavior is involved in some way,
    -- as the output of hspec will occasionally be swallowed entirely. I have
    -- not been able to observe this phenomenon on any other version of GHC,
    -- so I will simply disable this test on 9.0.
    prop "should throw an Underflow exception on negative inputs" $
      \(Negative (i :: Integer)) ->
        reifyNat i (evaluate . reflect) `shouldThrow` (== Underflow)
#endif
#if MIN_VERSION_base(4,10,0)
    it "should reflect very large Naturals correctly" $ do -- #41
      let d42, d2_63, d2_64 :: Natural
          d42   = 42
          d2_63 = 2^(63 :: Natural)
          d2_64 = 2^(64 :: Natural)
      reifyNat (toInteger d42)       $ \p -> natVal p `shouldBe` d42
      reifyNat (toInteger (d2_63-1)) $ \p -> natVal p `shouldBe` d2_63-1
      reifyNat (toInteger d2_63)     $ \p -> natVal p `shouldBe` d2_63
      reifyNat (toInteger (d2_64-1)) $ \p -> natVal p `shouldBe` d2_64-1
      reifyNat (toInteger d2_64)     $ \p -> natVal p `shouldBe` d2_64
#endif
