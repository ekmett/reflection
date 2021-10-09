-- Example of a dynamically generated FromJSON instance.
--
-- Can be useful when one needs to use a function with a
-- FromJSON constraint, but some detail about the
-- conversion from JSON is not known until runtime.
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson             -- from aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
#endif
import Data.Aeson.Types (Parser)
import Data.Proxy             -- from tagged
import Data.Reflection        -- from reflection
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup         -- from base
#endif
import Data.Text (Text, pack) -- from text

import Control.Applicative

-- These imports are only for constructing the example value
import Lens.Micro.Aeson (_Value)   -- form microlens-aeson
import Lens.Micro.Extras (preview) -- from microlens

data Foo = Foo
    {
        field1 :: Int
    ,   field2 :: Int
    } deriving (Show)

fooParser :: Text -> Object -> Parser Foo
fooParser prefix o = do
    Foo <$> o .: (toKey prefix <> "field1") <*> o .: (toKey prefix <> "field2")

#if MIN_VERSION_aeson(2,0,0)
toKey :: Text -> Key.Key
toKey = Key.fromText
#else
toKey :: Text -> Text
toKey = id
#endif

-- A wrapper over Foo carrying a phantom type s
newtype J a s = J { runJ :: a }

-- If the phantom type s reifies the parsing function, we can
-- use reflect to recover the function and implement
-- our FromJSON instance for J.
instance Reifies s (Object -> Parser a) => FromJSON (J a s) where
    parseJSON (Object v) = J <$> reflect (Proxy :: Proxy s) v
    parseJSON _          = empty

-- Convince the compiler that the phantom type in the proxy
-- supplied by reify is the same as the phantom type in J.
--
-- Otherwise the FromJSON instance for J won't kick in.
asProxyJ :: Proxy s -> J a s -> J a s
asProxyJ _ = id

exampleJSON :: Value
exampleJSON = maybe Null id (preview _Value str)
  where
    str = "{ \"zzfield1\" : 5, \"zzfield2\" : 7 }"::Text

main :: IO ()
main = do
    putStrLn "Enter prefix for the fields: "
    -- "zz" must be entered for the parse to succeed
    prefix <- fmap pack getLine

    -- fromJSON uses the dynamically generated FromJSON instance
    let result = reify (fooParser prefix) $ \proxy ->
            -- We must eliminate the J newtype before returning
            -- because, thanks to parametricity,
            -- the phantom type cannot escape the callback.
            runJ . asProxyJ proxy <$> fromJSON exampleJSON

    putStrLn (show (result :: Result Foo))
