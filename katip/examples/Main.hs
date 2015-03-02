{-# LANGUAGE TemplateHaskell #-}

module Main where

-------------------------------------------------------------------------------
import           Data.Aeson

import           Data.Text
-------------------------------------------------------------------------------
import           Katip
-------------------------------------------------------------------------------



main :: IO ()
main = do
    logM "example" Info "Easy to emit from IO directly!"
    logF myContext "example" Info "Here's a more stateful item."
    $(logT) myContext "example" Info "Here's one with code location."



myContext = MyContext "blah" 3

data MyContext = MyContext {
      foo :: Text
    , bar :: Int
    }


instance ToJSON MyContext where
    toJSON mc = object ["foo" .= foo mc, "bar" .= bar mc]

instance LogContext MyContext where
    payloadKeys _ mc = AllKeys



