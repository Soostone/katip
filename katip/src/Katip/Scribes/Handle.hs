{-# LANGUAGE RecordWildCards #-}

module Katip.Scribes.Handle where

-------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict     as HM
import           Data.Monoid
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.IO       as T
import           Data.Time
import qualified Data.Time.Locale.Compat as LC
import           System.IO
import           System.IO.Unsafe        (unsafePerformIO)
-------------------------------------------------------------------------------
import           Katip.Core
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
brackets :: Builder -> Builder
brackets m = fromText "[" <> m <> fromText "]"


-------------------------------------------------------------------------------
getKeys :: LogItem s => Verbosity -> s -> [Builder]
getKeys verb a = payloadObject verb a ^..
              to HM.toList . traverse . to rendPair
  where
    rendPair (k,v) = fromText k <> fromText ":" <> (v ^. _Primitive . to renderPrim)


-------------------------------------------------------------------------------
renderPrim :: Primitive -> Builder
renderPrim (StringPrim t) = fromText t
renderPrim (NumberPrim s) = fromString (show s)
renderPrim (BoolPrim b) = fromString (show b)
renderPrim NullPrim = fromText "null"


-------------------------------------------------------------------------------
data ColorStrategy
    = ColorLog Bool
    -- ^ Whether to use color control chars in log output
    | ColorIfTerminal
    -- ^ Color if output is a terminal


-------------------------------------------------------------------------------
-- | Logs to a file handle such as stdout, stderr, or a file.
mkHandleScribe :: ColorStrategy -> Handle -> Severity -> Verbosity -> IO Scribe
mkHandleScribe cs h sev verb = do
    hSetBuffering h LineBuffering
    colorize <- case cs of
      ColorIfTerminal -> hIsTerminalDevice h
      ColorLog b -> return b
    return $ Scribe $ \ i@Item{..} -> do
      when (_itemSeverity >= sev) $
        T.hPutStrLn h $ toLazyText $ formatItem colorize verb i


-------------------------------------------------------------------------------
formatItem :: LogItem a => Bool -> Verbosity -> Item a -> Builder
formatItem withColor verb Item{..} =
    brackets nowStr <>
    brackets (mconcat $ map fromText $ intercalateNs _itemNamespace) <>
    brackets (fromText (renderSeverity' _itemSeverity)) <>
    brackets (fromString _itemHost) <>
    brackets (fromString (show _itemProcess)) <>
    brackets (fromText (getThreadIdText _itemThread)) <>
    mconcat ks <>
    maybe mempty (brackets . fromString . locationToString) _itemLoc <>
    fromText " " <> (unLogStr _itemMessage)
  where
    nowStr = fromString $ formatTime LC.defaultTimeLocale "%Y-%m-%d %H:%M:%S" _itemTime
    ks = map brackets $ getKeys verb _itemPayload
    renderSeverity' s = case s of
      EmergencyS -> red $ renderSeverity s
      AlertS     -> red $ renderSeverity s
      CriticalS  -> red $ renderSeverity s
      ErrorS     -> red $ renderSeverity s
      WarningS   -> yellow $ renderSeverity s
      _         -> renderSeverity s
    red = colorize "31"
    yellow = colorize "33"
    colorize c s
      | withColor = "\ESC["<> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s


-------------------------------------------------------------------------------
-- | An implicit environment to enable logging directly ouf of the IO monad.
_ioLogEnv :: LogEnv
_ioLogEnv = unsafePerformIO $ do
    le <- initLogEnv "io" "io"
    lh <- mkHandleScribe ColorIfTerminal stdout DebugS V3
    return $ registerScribe "stdout" lh le
{-# NOINLINE _ioLogEnv #-}


-- -------------------------------------------------------------------------------
-- -- | A default IO instance to make prototype development easy. User
-- -- your own 'Monad' for production.
-- instance Katip IO where getLogEnv = return _ioLogEnv
