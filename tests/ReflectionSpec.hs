{-# LANGUAGE ScopedTypeVariables #-}
module ReflectionSpec where

import Control.Exception (evaluate)
import System.Mem (performGC)
import Data.Dynamic (dynTypeRep, fromDynamic, toDyn)
import Data.Proxy (Proxy(..))
import Data.Reflection
import Data.Typeable (cast, typeRep)
import GHC.TypeLits (symbolVal)
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "reifyTypeable" $ do
    prop "recovers a reflected value through Dynamic" $ \(a :: Int) ->
      reifyTypeable a $ \(p :: Proxy s) ->
        case fromDynamic (toDyn (reflect p, p)) of
          Just (b, q :: Proxy s) -> b == a && reflect q == a
          Nothing -> False
    it "supports type representations and casts" $
      reifyTypeable (1 :: Int) $ \(p :: Proxy s) -> do
        dynTypeRep (toDyn p) `shouldBe` typeRep (Proxy :: Proxy (Proxy s))
        (cast p :: Maybe (Proxy s)) `shouldBe` Just p
        (cast p :: Maybe Int) `shouldBe` Nothing
    it "does not force an unused value" $
      reifyTypeable (error "unused" :: Int) (const ()) `shouldBe` ()

    it "retains a lazy value after the continuation returns" $ do
      action <- evaluate $ reifyTypeable ([1,2,3] ++ error "unused" :: [Int]) $ \p ->
        return (take 3 (reflect p)) :: IO [Int]
      performGC
      action `shouldReturn` [1,2,3]
      performGC
      action `shouldReturn` [1,2,3]

  describe "reifySymbol" $
    prop "recovers the symbol through both interfaces" $ \s ->
      reifySymbol s (\p -> (symbolVal p, reflect p)) == (s, s)

  describe "explicit numeric reflection" $
    it "reflects zero, doubling, successor and predecessor" $
      ( reflect (Proxy :: Proxy Z)
      , reflect (Proxy :: Proxy (D (SD Z)))
      , reflect (Proxy :: Proxy (SD (D (SD Z))))
      , reflect (Proxy :: Proxy (PD (D (PD Z))))
      ) `shouldBe` (0 :: Int, 2 :: Int, 5 :: Int, -5 :: Int)

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
