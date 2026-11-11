{-# LANGUAGE ScopedTypeVariables #-}
module ReifySpec where

import Control.Exception (evaluate)
import Data.Reflection
import System.Mem (performGC)
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "reify" $ do
    prop "reflects the reified value" $ \(a :: Int) ->
      reify a reflect == a
    prop "keeps nested reifications distinct" $ \(a :: Int) (b :: Int) ->
      reify a (\p -> reify b (\q -> (reflect p, reflect q))) == (a, b)
    it "reflects a function" $
      reify ((+ 1) :: Int -> Int) (\p -> map (reflect p) [1,2,3]) `shouldBe` [2,3,4]
    it "preserves a lazy value" $
      reify (1 : error "unused" :: [Int]) (\p -> take 1 (reflect p)) `shouldBe` [1]
    it "does not force an unused value" $
      reify (error "unused" :: Int) (const ()) `shouldBe` ()
    it "does not force a value in a lazy result" $ do
      let xs = reify (error "unused" :: Int) (\p -> [reflect p])
      length xs `shouldBe` 1
      evaluate (sum xs) `shouldThrow` errorCall "unused"
    it "retains values after the continuation returns" $ do
      actions <- mapM (\a -> evaluate $ reify a (\p -> return (reflect p) :: IO Int)) [1..100]
      performGC
      sequence actions `shouldReturn` [1..100]
      performGC
      sequence (reverse actions) `shouldReturn` [100,99..1]
