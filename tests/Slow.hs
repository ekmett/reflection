module Main where

import qualified ReflectionSpec
import qualified ReifySpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  ReifySpec.spec
  ReflectionSpec.spec
