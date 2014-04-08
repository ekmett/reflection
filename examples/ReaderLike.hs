-- The UndecidableInstances here is benign, just for the "advanced"
-- example at the end.
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}

-- I don't demonstrate the advantages over implicit parameters here,
-- like multiple reifications of the same thing coexisting in
-- different, statically-checked contexts, etc.; this is intended as a
-- comparison to Reader(T).

import Data.Proxy
import Data.Reflection
import Control.Applicative
import System.IO

data MyConfig = MyConfig
    { magic  :: Bool
    , volume :: Integer
    }

data Report p = Report
    { magicality :: String
    , loud :: Bool
    } deriving (Show)

-- some arbitrary thing we do calculations with
newtype Datum p = Datum Integer deriving (Read, Show)

report :: forall p. (Reifies p MyConfig) => Report p
report = Report
    { magicality = if magic conf then "Magical." else "Not so magical..."
    , loud = volume conf >= 11
    }
  where
    conf = reflect (Proxy :: Proxy p)

calculate :: forall p. (Reifies p MyConfig) => Datum p -> Datum p -> Datum p
calculate (Datum m) (Datum n) = Datum ((m+n) * volume conf)
  where
    conf = reflect (Proxy :: Proxy p)

run :: forall p. (Reifies p MyConfig) => Proxy p -> IO ()
run p = do
    d1 <- ask "Datum 1:" :: IO (Datum p)
    d2 <- ask "Datum 2:" :: IO (Datum p)
    -- look ma, no plumbing
    print $ calculate d1 (calculate d2 d1)
    print (report :: Report p)

ask :: (Read s) => String -> IO s
ask prompt = do
    putStr prompt
    putChar ' '
    hFlush stdout
    readLn

main :: IO ()
main = do
    conf <- MyConfig <$> ask "Magic?" <*> ask "Volume:"
    reify conf run

-- If you're feeling adventurous, here is something we could not do
-- with ReaderT:
instance (Reifies p MyConfig) => Num (Datum p) where
    (+) = calculate
    m * n
        | magic conf   = m + n
        | otherwise    = Datum 0  -- sorry, no magic for you.
      where
        conf = reflect (Proxy :: Proxy p)
    abs = undefined
    signum = undefined
    fromInteger = Datum
