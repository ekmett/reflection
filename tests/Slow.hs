module Main where

import qualified ReifySpec
import Test.Hspec

main :: IO ()
main = hspec ReifySpec.spec
