{-# LANGUAGE TemplateHaskell #-}

module Main where

-------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text
-------------------------------------------------------------------------------
import           Katip
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------



main :: IO ()
main = runKatipT _ioLogEnv $ do
    logM "example" InfoS "Easy to emit from IO directly!"
    logF myContext "example" InfoS "Here's a more stateful item."
    $(logT) myContext "example" InfoS "Here's one with code location."



myContext = MyContext "blah" 3

data MyContext = MyContext {
      foo :: Text
    , bar :: Int
    }


instance ToJSON MyContext where
    toJSON mc = object ["foo" .= foo mc, "bar" .= bar mc]

instance LogContext MyContext where
    payloadKeys _ mc = AllKeys



