import Criterion.Main

import qualified Data.Reflection as Old
import qualified Data.NewReflection as New

old :: [Int] -> [Int]
old = map (\x -> Old.reify x Old.reflect)

new :: [Int] -> [Int]
new = map (\x -> New.reify x New.reflect)

main :: IO ()
main = defaultMain
    [ bench "old" $ nf old [1..100000]
    , bench "new" $ nf new [1..100000]
    ]
