{-# LANGUAGE RecordWildCards #-}

module Katip.Scribes.Handle
    ( mkHandleScribe
    ) where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8              as B
import qualified Data.HashMap.Strict                as HM
import           Data.Monoid
import           Data.Time
import           System.IO
import           System.IO.Unsafe                   (unsafePerformIO)
import           System.Locale
-------------------------------------------------------------------------------
import           Katip.Core
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
brackets :: Builder -> Builder
brackets m = fromByteString "[" <> m <> fromByteString "]"


-------------------------------------------------------------------------------
getKeys :: LogContext s => Verbosity -> s -> [Builder]
getKeys verb a =  payloadJson verb a ^..
              _Object . to HM.toList . traverse . to rendPair
  where
    rendPair (k,v) = fromText k <> fromText ":" <> (v ^. _Primitive . to renderPrim)


-------------------------------------------------------------------------------
renderPrim :: Primitive -> Builder
renderPrim (StringPrim t) = fromText t
renderPrim (NumberPrim s) = fromString (show s)
renderPrim (BoolPrim b) = fromString (show b)
renderPrim NullPrim = fromByteString "null"


-------------------------------------------------------------------------------
mkHandleScribe :: Handle -> Severity -> Verbosity -> IO Scribe
mkHandleScribe h sev verb = do
    hSetBuffering h LineBuffering
    return $ Scribe $ \ Item{..} -> do
      let nowStr = fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" itemTime
          ks = map brackets $ getKeys verb itemPayload
          msg = brackets nowStr <>
                brackets (mconcat $ map fromText $ intercalateNs itemNamespace) <>
                brackets (fromText (renderSeverity itemSeverity)) <>
                brackets (fromString itemHost) <>
                brackets (fromString (show itemProcess)) <>
                brackets (fromString (show itemThread)) <>
                mconcat ks <>
                maybe mempty (brackets . fromString . locationToString) itemLoc <>
                fromText " " <> fromText itemMessage
      if itemSeverity >= sev
        then B.hPutStrLn h $ toByteString msg
        else return ()



-------------------------------------------------------------------------------
-- | An implicit environment to enable logging directly ouf of the IO monad.
_ioLogEnv :: LogEnv
_ioLogEnv = unsafePerformIO $ do
    le <- initLogEnv "io" "io"
    lh <- mkHandleScribe stdout Debug V3
    return $ registerScribe "stdout" lh le
{-# NOINLINE _ioLogEnv #-}


-------------------------------------------------------------------------------
-- | A default IO instance to make prototype development easy. User
-- your own 'Monad' for production.
instance Katip IO where getLogEnv = return _ioLogEnv

