{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}    

import Data.Reflection.Tagged
import Text.Printf

-- | definition of the concepts

data Radius = Radius
data CentralMass = CentralMass

-- | you can calculate the orbital period of a planet given its
--   orbital radius and the mass of the central star.

period :: (Radius ::: Double, CentralMass ::: Double) => Double
period = 2 * pi / sqrt((the CentralMass) * 6.67384e-11 / (the Radius)**3)

-- | some utility values

year :: Double
year = 365*24*60*60

day :: Double
day = 24*60*60

main :: IO ()
main = do
  Radius `being` 1.4960e11 $ CentralMass `being` 1.9889e30 $
    printf "The orbital period of the Earth is %.2f years, or %.0f days\n" 
      (period/year) (period/day)

  Radius `being` 7.7843e11 $ CentralMass `being` 1.9889e30 $
    printf "The orbital period of Jupiter is %.1f years\n" $ period/year

  Radius `being` 1.27e11  $ CentralMass `being` 1.929e30 $
    printf "The orbital period of Kepler-22 b is %.0f days\n" $ period/day

{-

The orbital period of the Earth is 1.00 years, or 365 days
The orbital period of Jupiter is 11.9 years
The orbital period of Kepler-22 b is 290 days

-}
