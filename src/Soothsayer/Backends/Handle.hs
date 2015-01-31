{-# LANGUAGE RecordWildCards #-}
module Soothsayer.Backends.Handle where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Lens
import           Data.Aeson                         (ToJSON (..))
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8              as B
import           Data.Maybe
import           Data.Monoid
import           Data.Text                          (Text)
import           Data.Time
import           System.IO
import           System.Locale
-------------------------------------------------------------------------------
import           Soothsayer.Core
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
brackets :: Builder -> Builder
brackets m = fromByteString "[" <> m <> fromByteString "]"


-------------------------------------------------------------------------------
getKeys :: ToJSON s => s -> [Text] -> [Builder]
getKeys a keys = flip mapMaybe keys $ \ k ->
    a' ^? key k . _Primitive . to renderPrim
  where
    a' = toJSON a


-------------------------------------------------------------------------------
renderPrim (StringPrim t) = fromText t
renderPrim (NumberPrim s) = fromString (show s)
renderPrim (BoolPrim b) = fromString (show b)
renderPrim NullPrim = fromByteString "null"


-------------------------------------------------------------------------------
mkHandleHandler :: Handle -> [Text] -> IO LogHandler
mkHandleHandler h keys = do
    hSetBuffering h LineBuffering
    return $ LogHandler $ \ Item{..} -> do
      let nowStr = fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" itemTime
          ks = map brackets $ getKeys itemPayload keys
          msg = brackets nowStr <>
                brackets (fromText (renderSeverity itemSeverity)) <>
                brackets (fromString itemHost) <>
                brackets (fromString (show itemThread)) <>
                mconcat ks <>
                fromText " " <> fromText itemMessage
      B.putStrLn $ toByteString msg

