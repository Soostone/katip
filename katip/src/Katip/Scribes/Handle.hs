{-# LANGUAGE RecordWildCards #-}

module Katip.Scribes.Handle where

-------------------------------------------------------------------------------
import           Control.Applicative as A
import           Control.Monad
import           Control.Exception (onException)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.IO as T
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.Chan.Unagi.Bounded as U
import           Control.Concurrent.Async
-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Format.Time (formatAsLogTime)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
brackets :: Builder -> Builder
brackets m = fromText "[" <> m <> fromText "]"


-------------------------------------------------------------------------------
getKeys :: LogItem s => Verbosity -> s -> [Builder]
getKeys verb a = concat (renderPair A.<$> HM.toList (payloadObject verb a))
  where
    renderPair :: (Text, Value) -> [Builder]
    renderPair (k,v) =
      case v of
        Object o -> concat [renderPair (k <> "." <> k', v')  | (k', v') <- HM.toList o]
        String t -> [fromText (k <> ":" <> t)]
        Number n -> [fromText (k <> ":") <> fromString (show n)]
        Bool b -> [fromText (k <> ":") <> fromString (show b)]
        Null -> [fromText (k <> ":null")]
        _ -> mempty -- Can't think of a sensible way to handle arrays


-------------------------------------------------------------------------------
data ColorStrategy
    = ColorLog Bool
    -- ^ Whether to use color control chars in log output
    | ColorIfTerminal
    -- ^ Color if output is a terminal

-------------------------------------------------------------------------------
--TODO: probably drop this stuff since katip does buffering and this isn't doing concurrent writer workers?
data WorkerCmd =
    HNewItem Builder
  | HPoisonPill

-------------------------------------------------------------------------------
-- | Logs to a file handle such as stdout, stderr, or a file. Contexts
-- and other information will be flattened out into bracketed
-- fields. For example:
--
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:32:7] Started
-- > [2016-05-11 21:01:15][MyApp.confrabulation][Debug][myhost.example.com][1724][ThreadId 1154][confrab_factor:42.0][main:Helpers.Logging Helpers/Logging.hs:41:9] Confrabulating widgets, with extra namespace and context
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:43:7] Namespace and context are back to normal
--
-- Returns the newly-created `Scribe` together with a finaliser the user needs to run to perform resource cleanup.
mkHandleScribe :: ColorStrategy -> Handle -> Severity -> Verbosity -> IO (Scribe, IO ())
mkHandleScribe cs h sev verb = do
  (inChan, outChan) <- U.newChan 4096
  worker <- async $ workerLoop outChan
  flip onException (stopWorker worker inChan) $ do
    hSetBuffering h LineBuffering
    colorize <- case cs of
      ColorIfTerminal -> hIsTerminalDevice h
      ColorLog b -> return b
    let scribe = Scribe $ \i ->
          when (permitItem sev i) $ void (U.tryWriteChan inChan (HNewItem (formatItem colorize verb i)))
    return (scribe, stopWorker worker inChan)

  where
    stopWorker :: Async () -> U.InChan WorkerCmd -> IO ()
    stopWorker worker inChan = do
      U.writeChan inChan HPoisonPill
      void $ waitCatch worker

    workerLoop :: U.OutChan WorkerCmd -> IO ()
    workerLoop outChan = do
      newCmd <- U.readChan outChan
      case newCmd of
        HNewItem b  -> do
          T.hPutStrLn h $ toLazyText b
          workerLoop outChan
        HPoisonPill -> return ()

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
    nowStr = fromText (formatAsLogTime _itemTime)
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
-- Be careful as this LogEnv won't perform any resource cleanup for you.
_ioLogEnv :: LogEnv
_ioLogEnv = unsafePerformIO $ do
    le <- initLogEnv "io" "io"
    (lh, finalizer) <- mkHandleScribe ColorIfTerminal stdout DebugS V3
    registerScribe "stdout" lh (defaultScribeSettings finalizer) le
{-# NOINLINE _ioLogEnv #-}


-- -------------------------------------------------------------------------------
-- -- | A default IO instance to make prototype development easy. User
-- -- your own 'Monad' for production.
-- instance Katip IO where getLogEnv = return _ioLogEnv
