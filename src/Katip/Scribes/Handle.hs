{-# LANGUAGE RecordWildCards #-}

module Katip.Scribes.Handle
    ( mkHandleScribe
    -- * exported for benchmarking
    , mkHandleScribe'
    , bbFormat
    , colorFormat
    ) where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Lens
import           Control.Monad
import qualified Data.String as S
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8              as B
import qualified Data.HashMap.Strict                as HM
import           Data.Monoid
import           Data.Text.Lazy                          (pack, fromStrict, fromChunks)
import           Data.Text.Lazy.Encoding
import           Data.Time
import           System.IO
import           System.IO.Unsafe                   (unsafePerformIO)
import           System.Locale

import qualified Text.PrettyPrint.Leijen.Text       as PP
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
mkHandleScribe = mkHandleScribe' bbFormat


-------------------------------------------------------------------------------
mkHandleScribe' :: (forall a. LogContext a => Verbosity -> Item a -> Builder) -> Handle -> Severity -> Verbosity -> IO Scribe
mkHandleScribe' fmt h sev verb = do
    hSetBuffering h LineBuffering
    return $ Scribe $ \ i@Item{..} -> do
      when (itemSeverity >= sev) $
        B.hPutStrLn h $ toByteString $ fmt verb i


-------------------------------------------------------------------------------
bbFormat verb Item{..} =
    brackets nowStr <>
    brackets (mconcat $ map fromText $ intercalateNs itemNamespace) <>
    brackets (fromText (renderSeverity itemSeverity)) <>
    brackets (fromString itemHost) <>
    brackets (fromString (show itemProcess)) <>
    brackets (fromString (show itemThread)) <>
    mconcat ks <>
    maybe mempty (brackets . fromString . locationToString) itemLoc <>
    fromText " " <> fromText itemMessage
  where
    nowStr = fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" itemTime
    ks = map brackets $ getKeys verb itemPayload


-------------------------------------------------------------------------------
colorFormat verb Item{..} = disp $
    PP.brackets (PP.string nowStr) <>
    PP.brackets (PP.string $ fromChunks $ intercalateNs itemNamespace) <>
    PP.brackets (PP.string $ fromStrict (renderSeverity' itemSeverity)) <>
    PP.brackets (PP.string $ pack itemHost) <>
    PP.brackets (PP.string $ pack (show itemProcess)) <>
    PP.brackets (PP.string $ pack (show itemThread)) <>
    mconcat ks <>
    maybe mempty (PP.brackets . S.fromString . locationToString) itemLoc <>
    PP.space <> PP.string (fromStrict itemMessage)
  where
    disp = fromLazyByteString . encodeUtf8 . PP.displayT . PP.renderCompact
    nowStr = pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" itemTime
    ks = map (PP.brackets . PP.string . decodeUtf8 . toLazyByteString) $ getKeys verb itemPayload
    renderSeverity' s = case s of
      Emergency -> red $ renderSeverity s
      Alert     -> red $ renderSeverity s
      Critical  -> red $ renderSeverity s
      Error     -> red $ renderSeverity s
      Warning   -> yellow $ renderSeverity s
      _         -> renderSeverity s
    red = colorize "31"
    yellow = colorize "33"
    colorize c s = "\ESC["<> c <> "m" <> s <> "\ESC[0m"


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

