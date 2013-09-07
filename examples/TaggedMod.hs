{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

import Data.Reflection.Tagged
import Test.Hspec

data Mod = Mod

plus :: (Mod ::: Integer) => Integer -> Integer -> Integer 
plus x y = (x + y) `mod` the Mod

main :: IO ()
main = hspec $ do
  describe "Data.Reflection.Tagged" $ do
    it "can handle modulo state sharing properly." $ do
      (Mod `being` 10 $ plus 6 7) `shouldBe` 3
      (Mod `being`  2 $ plus 6 7) `shouldBe` 1
