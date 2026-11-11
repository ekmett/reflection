{-# LANGUAGE ScopedTypeVariables #-}
module ReflectionSpec where

import Data.Reflection
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reified monoids" $ do
    prop "foldBy agrees with concatenation" $ \(xs :: [[Int]]) ->
      foldBy (++) [] xs == concat xs
    prop "foldMapBy preserves the order of mapped values" $ \(xs :: [[Int]]) ->
      foldMapBy (++) [] reverse xs == concatMap reverse xs
    prop "reifyMonoid uses the supplied addition and identity" $ \(xs :: [Int]) ->
      reifyMonoid (+) 0 (foldMap ReflectedMonoid) xs == sum xs

  describe "reified applicatives" $ do
    it "reifyApplicative uses list application in order" $
      reifyApplicative pure (<*>) (traverse ReflectedApplicative)
        [[1,2],[3,4 :: Int]] `shouldBe` [[1,3],[1,4],[2,3],[2,4]]
    prop "traverseBy agrees with traverse for Maybe" $ \(xs :: [Int]) ->
      let f x = if even x then Just (x + 1) else Nothing
      in traverseBy Just (<*>) f xs == traverse f xs
    prop "sequenceBy agrees with sequenceA for Maybe" $ \(xs :: [Maybe Int]) ->
      sequenceBy Just (<*>) xs == sequenceA xs
    it "preserves short-circuiting during traversal" $
      traverseBy Just (<*>) (const Nothing) (1 : undefined :: [Int])
        `shouldBe` (Nothing :: Maybe [Int])
