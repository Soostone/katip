{-# LANGUAGE RecordWildCards #-}

module Katip.Scribes.Handle where

import           Control.Applicative    as A
import           Control.Concurrent
import           Control.Exception      (bracket_, finally)
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict    as HM
import           Data.Monoid
import           Data.Scientific        as S
import           Data.Text              (Text)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.IO      as T
import           System.IO

import           Katip.Core
import           Katip.Format.Time      (formatAsLogTime)


brackets :: Builder -> Builder
brackets m = fromText "[" <> m <> fromText "]"


getKeys :: LogItem s => Verbosity -> s -> [Builder]
getKeys verb a = concat (renderPair A.<$> HM.toList (payloadObject verb a))
  where
    renderPair :: (Text, Value) -> [Builder]
    renderPair (k,v) =
      case v of
        Object o -> concat [renderPair (k <> "." <> k', v')  | (k', v') <- HM.toList o]
        String t -> [fromText (k <> ":" <> t)]
        Number n -> [fromText (k <> ":") <> fromString (formatNumber n)]
        Bool b -> [fromText (k <> ":") <> fromString (show b)]
        Null -> [fromText (k <> ":null")]
        _ -> mempty -- Can't think of a sensible way to handle arrays
    formatNumber :: Scientific -> String
    formatNumber n =
      formatScientific Generic (if isFloating n then Nothing else Just 0) n


data ColorStrategy
    = ColorLog Bool
    -- ^ Whether to use color control chars in log output
    | ColorIfTerminal
    -- ^ Color if output is a terminal
  deriving (Show, Eq)

-- | Logs to a file handle such as stdout, stderr, or a file. Contexts
-- and other information will be flattened out into bracketed
-- fields. For example:
--
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:32:7] Started
-- > [2016-05-11 21:01:15][MyApp.confrabulation][Debug][myhost.example.com][1724][ThreadId 1154][confrab_factor:42.0][main:Helpers.Logging Helpers/Logging.hs:41:9] Confrabulating widgets, with extra namespace and context
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:43:7] Namespace and context are back to normal
--
-- Returns the newly-created `Scribe`. The finalizer flushes the
-- handle. Handle mode is set to 'LineBuffering' automatically.
mkHandleScribe :: ColorStrategy -> Handle -> Severity -> Verbosity -> IO Scribe
mkHandleScribe cs h sev verb = do
    hSetBuffering h LineBuffering
    colorize <- case cs of
      ColorIfTerminal -> hIsTerminalDevice h
      ColorLog b      -> return b
    lock <- newMVar ()
    let logger i@Item{..} = do
          when (permitItem sev i) $ bracket_ (takeMVar lock) (putMVar lock ()) $
            T.hPutStrLn h $ toLazyText $ formatItem colorize verb i
    return $ Scribe logger (hFlush h)


-------------------------------------------------------------------------------
-- | A specialization of 'mkHandleScribe' that takes a 'FilePath'
-- instead of a 'Handle'. It is responsible for opening the file in
-- 'AppendMode' and will close the file handle on
-- 'closeScribe'/'closeScribes'. Does not do log coloring. Sets handle
-- to 'LineBuffering' mode.
mkFileScribe :: FilePath -> Severity -> Verbosity -> IO Scribe
mkFileScribe f sev verb = do
  h <- openFile f AppendMode
  Scribe logger finalizer <- mkHandleScribe (ColorLog False) h sev verb
  return (Scribe logger (finalizer `finally` hClose h))


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
    nowStr = fromText (formatAsLogTime _itemTime)
    ks = map brackets $ getKeys verb _itemPayload
    renderSeverity' s = case s of
      EmergencyS -> red $ renderSeverity s
      AlertS     -> red $ renderSeverity s
      CriticalS  -> red $ renderSeverity s
      ErrorS     -> red $ renderSeverity s
      WarningS   -> yellow $ renderSeverity s
      _          -> renderSeverity s
    red = colorize "31"
    yellow = colorize "33"
    colorize c s
      | withColor = "\ESC["<> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s


-- | Provides a simple log environment with 1 scribe going to
-- stdout. This is a decent example of how to build a LogEnv and is
-- best for scripts that just need a quick, reasonable set up to log
-- to stdout.
ioLogEnv :: Severity -> Verbosity -> IO LogEnv
ioLogEnv sev verb = do
  le <- initLogEnv "io" "io"
  lh <- mkHandleScribe ColorIfTerminal stdout sev verb
  registerScribe "stdout" lh defaultScribeSettings le
