module Katip.Scribes.Handle where

-------------------------------------------------------------------------------
import Control.Applicative as A
import Control.Concurrent
import Control.Exception (bracket_, finally)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Monoid as M
import Data.Scientific as S
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO as T
-------------------------------------------------------------------------------
import Katip.Core
import Katip.Format.Time (formatAsLogTime)
import System.IO

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
brackets :: Builder -> Builder
brackets m = fromText "[" M.<> m <> fromText "]"

-------------------------------------------------------------------------------
getKeys :: LogItem s => Verbosity -> s -> [Builder]
getKeys verb a = concat (renderPair A.<$> HM.toList (payloadObject verb a))
  where
    renderPair :: (Text, Value) -> [Builder]
    renderPair (k, v) =
      case v of
        Object o -> concat [renderPair (k <> "." <> k', v') | (k', v') <- HM.toList o]
        String t -> [fromText (k <> ":" <> t)]
        Number n -> [fromText (k <> ":") <> fromString (formatNumber n)]
        Bool b -> [fromText (k <> ":") <> fromString (show b)]
        Null -> [fromText (k <> ":null")]
        _ -> mempty -- Can't think of a sensible way to handle arrays
    formatNumber :: Scientific -> String
    formatNumber n =
      formatScientific Generic (if isFloating n then Nothing else Just 0) n

-------------------------------------------------------------------------------
data ColorStrategy
  = -- | Whether to use color control chars in log output
    ColorLog Bool
  | -- | Color if output is a terminal
    ColorIfTerminal
  deriving (Show, Eq)

-------------------------------------------------------------------------------

-- | Logs to a file handle such as stdout, stderr, or a file. Contexts
-- and other information will be flattened out into bracketed
-- fields. For example:
--
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][PID 1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:32:7] Started
-- > [2016-05-11 21:01:15][MyApp.confrabulation][Debug][myhost.example.com][PID 1724][ThreadId 1154][confrab_factor:42.0][main:Helpers.Logging Helpers/Logging.hs:41:9] Confrabulating widgets, with extra namespace and context
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][PID 1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:43:7] Namespace and context are back to normal
--
-- Returns the newly-created `Scribe`. The finalizer flushes the
-- handle. Handle mode is set to 'LineBuffering' automatically.
mkHandleScribe :: ColorStrategy -> Handle -> PermitFunc -> Verbosity -> IO Scribe
mkHandleScribe = mkHandleScribeWithFormatter bracketFormat

-- | Logs to a file handle such as stdout, stderr, or a file. Takes a custom
-- `ItemFormatter` that can be used to format `Item` as needed.
--
-- Returns the newly-created `Scribe`. The finalizer flushes the
-- handle. Handle mode is set to 'LineBuffering' automatically.
mkHandleScribeWithFormatter ::
  (forall a. LogItem a => ItemFormatter a) ->
  ColorStrategy ->
  Handle ->
  PermitFunc ->
  Verbosity ->
  IO Scribe
mkHandleScribeWithFormatter itemFormatter cs h permitF verb = do
  hSetBuffering h LineBuffering
  colorize <- case cs of
    ColorIfTerminal -> hIsTerminalDevice h
    ColorLog b -> return b
  lock <- newMVar ()
  let logger i@Item {..} = do
        bracket_ (takeMVar lock) (putMVar lock ()) $
          T.hPutStrLn h $ toLazyText $ itemFormatter colorize verb i
  return $ Scribe logger (hFlush h) permitF

-------------------------------------------------------------------------------

-- | A specialization of 'mkHandleScribe' that takes a 'FilePath'
-- instead of a 'Handle'. It is responsible for opening the file in
-- 'AppendMode' and will close the file handle on
-- 'closeScribe'/'closeScribes'. Does not do log coloring. Sets handle
-- to 'LineBuffering' mode.
mkFileScribe :: FilePath -> PermitFunc -> Verbosity -> IO Scribe
mkFileScribe f permitF verb = do
  h <- openFile f AppendMode
  Scribe logger finalizer permit <- mkHandleScribe (ColorLog False) h permitF verb
  return (Scribe logger (finalizer `finally` hClose h) permit)

-------------------------------------------------------------------------------

-- | A custom ItemFormatter for logging `Item`s. Takes a `Bool` indicating
-- whether to colorize the output, `Verbosity` of output, and an `Item` to
-- format.
--
-- See `bracketFormat` and `jsonFormat` for examples.
type ItemFormatter a = Bool -> Verbosity -> Item a -> Builder

formatItem :: LogItem a => ItemFormatter a
formatItem = bracketFormat
{-# DEPRECATED formatItem "Use bracketFormat instead" #-}

-- | A traditional 'bracketed' log format. Contexts and other information will
-- be flattened out into bracketed fields. For example:
--
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][PID 1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:32:7] Started
-- > [2016-05-11 21:01:15][MyApp.confrabulation][Debug][myhost.example.com][PID 1724][ThreadId 1154][confrab_factor:42.0][main:Helpers.Logging Helpers/Logging.hs:41:9] Confrabulating widgets, with extra namespace and context
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][PID 1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:43:7] Namespace and context are back to normal
bracketFormat :: LogItem a => ItemFormatter a
bracketFormat withColor verb Item {..} =
  brackets nowStr
    <> brackets (mconcat $ map fromText $ intercalateNs _itemNamespace)
    <> brackets (fromText (renderSeverity' _itemSeverity))
    <> brackets (fromString _itemHost)
    <> brackets ("PID " <> fromString (show _itemProcess))
    <> brackets ("ThreadId " <> fromText (getThreadIdText _itemThread))
    <> mconcat ks
    <> maybe mempty (brackets . fromString . locationToString) _itemLoc
    <> fromText " "
    <> (unLogStr _itemMessage)
  where
    nowStr = fromText (formatAsLogTime _itemTime)
    ks = map brackets $ getKeys verb _itemPayload
    renderSeverity' severity =
      colorBySeverity withColor severity (renderSeverity severity)

-- | Logs items as JSON. This can be useful in circumstances where you already
-- have infrastructure that is expecting JSON to be logged to a standard stream
-- or file. For example:
--
-- > {"at":"2018-10-02T21:50:30.4523848Z","env":"production","ns":["MyApp"],"data":{},"app":["MyApp"],"msg":"Started","pid":"10456","loc":{"loc_col":9,"loc_pkg":"main","loc_mod":"Helpers.Logging","loc_fn":"Helpers\\Logging.hs","loc_ln":44},"host":"myhost.example.com","sev":"Info","thread":"ThreadId 139"}
-- > {"at":"2018-10-02T21:50:30.4523848Z","env":"production","ns":["MyApp","confrabulation"],"data":{"confrab_factor":42},"app":["MyApp"],"msg":"Confrabulating widgets, with extra namespace and context","pid":"10456","loc":{"loc_col":11,"loc_pkg":"main","loc_mod":"Helpers.Logging","loc_fn":"Helpers\\Logging.hs","loc_ln":53},"host":"myhost.example.com","sev":"Debug","thread":"ThreadId 139"}
-- > {"at":"2018-10-02T21:50:30.4523848Z","env":"production","ns":["MyApp"],"data":{},"app":["MyApp"],"msg":"Namespace and context are back to normal","pid":"10456","loc":{"loc_col":9,"loc_pkg":"main","loc_mod":"Helpers.Logging","loc_fn":"Helpers\\Logging.hs","loc_ln":55},"host":"myhost.example.com","sev":"Info","thread":"ThreadId 139"}
jsonFormat :: LogItem a => ItemFormatter a
jsonFormat withColor verb i =
  fromText $
    colorBySeverity withColor (_itemSeverity i) $
      toStrict $ decodeUtf8 $ encode $ itemJson verb i

-- | Color a text message based on `Severity`. `ErrorS` and more severe errors
-- are colored red, `WarningS` is colored yellow, and all other messages are
-- rendered in the default color.
colorBySeverity :: Bool -> Severity -> Text -> Text
colorBySeverity withColor severity msg = case severity of
  EmergencyS -> red msg
  AlertS -> red msg
  CriticalS -> red msg
  ErrorS -> red msg
  WarningS -> yellow msg
  _ -> msg
  where
    red = colorize "31"
    yellow = colorize "33"
    colorize c s
      | withColor = "\ESC[" <> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s

-- | Provides a simple log environment with 1 scribe going to
-- stdout. This is a decent example of how to build a LogEnv and is
-- best for scripts that just need a quick, reasonable set up to log
-- to stdout.
ioLogEnv :: PermitFunc -> Verbosity -> IO LogEnv
ioLogEnv permit verb = do
  le <- initLogEnv "io" "io"
  lh <- mkHandleScribe ColorIfTerminal stdout permit verb
  registerScribe "stdout" lh defaultScribeSettings le
