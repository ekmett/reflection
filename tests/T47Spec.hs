{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A regression test for issue #47.
module T47Spec where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Reflection
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Given" $ do
    it "should give Normal properly" $
      give Normal (toJSON (Foo Bar)) `shouldBe`
      Object (M.fromList [("Foo",String "Bar")])
    it "should give ViaShow properly" $
      give ViaShow (toJSON (Foo Bar)) `shouldBe`
      Object (M.fromList [("Foo",String "SHOWBAR")])

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

data Foo = Foo Bar

instance Show Foo where
  show _ = "SHOWFOO"

data Bar = Bar | BarBar

instance Show Bar where
  show _ = "SHOWBAR"

----------------------------------------------------------------------------
-- ToJSON instances
----------------------------------------------------------------------------

instance Given Style => ToJSON Foo where
  toJSON (Foo x) = Object $ M.singleton "Foo" (toJSON x)

instance Given Style => ToJSON Bar where
  toJSON x = case given of
    Normal -> String $ case x of
                Bar    -> "Bar"
                BarBar -> "BarBar"
    ViaShow -> String $ show x

data Style = Normal | ViaShow

----------------------------------------------------------------------------
-- Minimized aeson
----------------------------------------------------------------------------

class ToJSON a where
  toJSON :: a -> Value

data Value
  = Object !(Map String Value)
  | String !String
  deriving (Eq, Show)
