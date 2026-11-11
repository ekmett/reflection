{-# LANGUAGE CPP #-}
#ifdef MIN_VERSION_template_haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
#endif
module THSpec where

#ifdef MIN_VERSION_template_haskell
import Data.Proxy
import Data.Reflection
import Language.Haskell.TH (runQ)
#endif
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
#ifdef MIN_VERSION_template_haskell
spec =
  describe "Template Haskell reflection" $ do
    it "reflects signed integer splices" $
      reflect (Proxy :: Proxy $(int (-5))) `shouldBe` (negate 5 :: Int)
    it "reflects natural splices" $
      (reflect (Proxy :: Proxy $(nat 0)), reflect (Proxy :: Proxy $(nat 5)))
        `shouldBe` (0 :: Int, 5 :: Int)
    it "reflects numeric expression splices" $
      reflect $(2 + 3) `shouldBe` (5 :: Integer)
    it "rejects negative naturals" $
      runQ (nat (-1)) `shouldThrow` errorCall "nat: negative"
#else
spec = return ()
#endif
