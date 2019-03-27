{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
#if MIN_VERSION_base(4, 9, 0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
-- | This module is not meant to be imported directly and may contain
-- internal mechanisms that will change without notice.
module Katip.Core where

-------------------------------------------------------------------------------
import           Control.Applicative               as A
import           Control.AutoUpdate
import           Control.Concurrent
import qualified Control.Concurrent.Async          as Async
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue    as BQ
import           Control.Exception.Safe
import           Control.Monad                     (unless, void, when)
import           Control.Monad.Base
#if MIN_VERSION_base(4, 9, 0)
import qualified Control.Monad.Fail                as MF
#endif
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
#if !MIN_VERSION_either(4, 5, 0)
import           Control.Monad.Trans.Either
#endif
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource      (ResourceT, transResourceT)
import           Control.Monad.Trans.RWS.Lazy      (RWST, mapRWST)
import qualified Control.Monad.Trans.RWS.Strict    as Strict (RWST, mapRWST)
import           Control.Monad.Trans.State.Lazy    (StateT, mapStateT)
import qualified Control.Monad.Trans.State.Strict  as Strict (StateT, mapStateT)
import           Control.Monad.Trans.Writer.Lazy   (WriterT, mapWriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT,
                                                              mapWriterT)
import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    object)
import qualified Data.Aeson                        as A
import           Data.Foldable                     as FT
import qualified Data.HashMap.Strict               as HM
import           Data.List
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (fromMaybe)
import           Data.Semigroup                    as SG
import           Data.String
import           Data.String.Conv
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy.Builder            as B
import           Data.Time
import           GHC.Generics                      hiding (to)
#if MIN_VERSION_base(4, 8, 0)
#if !MIN_VERSION_base(4, 9, 0)
import           GHC.SrcLoc
#endif
import           GHC.Stack
#endif
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax        as TH
import           Lens.Micro
import           Lens.Micro.TH
import           Network.HostName
#if mingw32_HOST_OS
import           Katip.Compat
#else
import           System.Posix
#endif

-------------------------------------------------------------------------------


readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
              [x] -> Just x
              []  -> Nothing -- no parse
              _   -> Nothing -- Ambiguous parse


-------------------------------------------------------------------------------
-- | Represents a heirarchy of namespaces going from general to
-- specific. For instance: ["processname", "subsystem"]. Note that
-- single-segment namespaces can be created using
-- IsString/OverloadedStrings, so "foo" will result in Namespace
-- ["foo"].
newtype Namespace = Namespace { unNamespace :: [Text] }
  deriving (Eq,Show,Read,Ord,Generic,ToJSON,FromJSON,SG.Semigroup,Monoid)

instance IsString Namespace where
    fromString s = Namespace [fromString s]


-------------------------------------------------------------------------------
-- | Ready namespace for emission with dots to join the segments.
intercalateNs :: Namespace -> [Text]
intercalateNs (Namespace xs) = intersperse "." xs


-------------------------------------------------------------------------------
-- | Application environment, like @prod@, @devel@, @testing@.
newtype Environment = Environment { getEnvironment :: Text }
  deriving (Eq,Show,Read,Ord,Generic,ToJSON,FromJSON,IsString)


-------------------------------------------------------------------------------
data Severity
    = DebugS                   -- ^ Debug messages
    | InfoS                    -- ^ Information
    | NoticeS                  -- ^ Normal runtime Conditions
    | WarningS                 -- ^ General Warnings
    | ErrorS                   -- ^ General Errors
    | CriticalS                -- ^ Severe situations
    | AlertS                   -- ^ Take immediate action
    | EmergencyS               -- ^ System is unusable
  deriving (Eq, Ord, Show, Read, Generic, Enum, Bounded)


-------------------------------------------------------------------------------
-- | Verbosity controls the amount of information (columns) a 'Scribe'
-- emits during logging.
--
-- The convention is:
-- - 'V0' implies no additional payload information is included in message.
-- - 'V3' implies the maximum amount of payload information.
-- - Anything in between is left to the discretion of the developer.
data Verbosity = V0 | V1 | V2 | V3
  deriving (Eq, Ord, Show, Read, Generic, Enum, Bounded)


-------------------------------------------------------------------------------
renderSeverity :: Severity -> Text
renderSeverity s = case s of
      DebugS     -> "Debug"
      InfoS      -> "Info"
      NoticeS    -> "Notice"
      WarningS   -> "Warning"
      ErrorS     -> "Error"
      CriticalS  -> "Critical"
      AlertS     -> "Alert"
      EmergencyS -> "Emergency"


-------------------------------------------------------------------------------
textToSeverity :: Text -> Maybe Severity
textToSeverity = go . T.toLower
  where
    go "debug"     = Just DebugS
    go "info"      = Just InfoS
    go "notice"    = Just NoticeS
    go "warning"   = Just WarningS
    go "error"     = Just ErrorS
    go "critical"  = Just CriticalS
    go "alert"     = Just AlertS
    go "emergency" = Just EmergencyS
    go _           = Nothing


instance ToJSON Severity where
    toJSON s = A.String (renderSeverity s)

instance FromJSON Severity where
    parseJSON = A.withText "Severity" parseSeverity
      where
        parseSeverity t = case textToSeverity t of
          Just x  -> return x
          Nothing -> fail $ "Invalid Severity " ++ toS t

instance ToJSON Verbosity where
    toJSON s = A.String $ case s of
      V0 -> "V0"
      V1 -> "V1"
      V2 -> "V2"
      V3 -> "V3"

instance FromJSON Verbosity where
    parseJSON = A.withText "Verbosity" $ \s -> case s of
      "V0" -> return V0
      "V1" -> return V1
      "V2" -> return V2
      "V3" -> return V3
      _    -> fail $ "Invalid Verbosity " ++ toS s


-------------------------------------------------------------------------------
-- | Log message with Builder underneath; use '<>' to concat in O(1).
newtype LogStr = LogStr { unLogStr :: B.Builder }
    deriving (Generic, Show, Eq)

instance IsString LogStr where
    fromString = LogStr . B.fromString


instance Semigroup LogStr where
  (LogStr a) <> (LogStr b) = LogStr (a <> b)


instance Monoid LogStr where
    mappend = (<>)
    mempty = LogStr mempty


instance FromJSON LogStr where
    parseJSON = A.withText "LogStr" parseLogStr
      where
        parseLogStr = return . LogStr . B.fromText

-------------------------------------------------------------------------------
-- | Pack any string-like thing into a 'LogStr'. This will
-- automatically work on 'String', 'ByteString', 'Text' and any of the
-- lazy variants.
logStr :: StringConv a Text => a -> LogStr
logStr t = LogStr (B.fromText $ toS t)


-------------------------------------------------------------------------------
-- | Shorthand for 'logStr'
ls :: StringConv a Text => a -> LogStr
ls = logStr


-------------------------------------------------------------------------------
-- | Convert any showable type into a 'LogStr'.
showLS :: Show a => a -> LogStr
showLS = ls . show


-------------------------------------------------------------------------------
newtype ThreadIdText = ThreadIdText {
      getThreadIdText :: Text
    } deriving (ToJSON, FromJSON, Show, Eq, Ord)


mkThreadIdText :: ThreadId -> ThreadIdText
mkThreadIdText = ThreadIdText . stripPrefix' "ThreadId " . T.pack . show
  where
    stripPrefix' pfx t = fromMaybe t (T.stripPrefix pfx t)


-------------------------------------------------------------------------------
-- | This has everything each log message will contain.
data Item a = Item {
      _itemApp       :: Namespace
    , _itemEnv       :: Environment
    , _itemSeverity  :: Severity
    , _itemThread    :: ThreadIdText
    , _itemHost      :: HostName
    , _itemProcess   :: ProcessID
    , _itemPayload   :: a
    , _itemMessage   :: LogStr
    , _itemTime      :: UTCTime
    , _itemNamespace :: Namespace
    , _itemLoc       :: Maybe Loc
    } deriving (Generic, Functor)
makeLenses ''Item

-- Manual instance because 'Loc' has no 'Eq' and 'Show' instances in old
-- versions of template-haskell (< 2.10)
instance Eq a => Eq (Item a) where
    a == b = FT.and [ _itemApp a == _itemApp b
                    , _itemEnv a == _itemEnv b
                    , _itemSeverity a == _itemSeverity b
                    , _itemThread a == _itemThread b
                    , _itemHost a == _itemHost b
                    , _itemProcess a == _itemProcess b
                    , _itemPayload a == _itemPayload b
                    , _itemMessage a == _itemMessage b
                    , _itemTime a == _itemTime b
                    , _itemNamespace a == _itemNamespace b
                    , case (_itemLoc a, _itemLoc b) of
                        (Nothing, Nothing) -> True
                        (Just l1, Just l2) -> FT.and [ loc_filename l1 == loc_filename l2
                                                     , loc_package l1 == loc_package l2
                                                     , loc_module l1 == loc_module l2
                                                     , loc_start l1 == loc_start l2
                                                     , loc_end l1 == loc_end l2
                                                     ]
                        _ -> False
                    ]

instance Show a => Show (Item a) where
    showsPrec d Item{..} = showParen (d >= 11) ( showString "Item {"
                                               . field "_itemApp" _itemApp
                                               . field "_itemEnv" _itemEnv
                                               . field "_itemSeverity" _itemSeverity
                                               . field "_itemThread" _itemThread
                                               . field "_itemHost" _itemHost
                                               . field "_itemProcess" _itemProcess
                                               . field "_itemPayload" _itemPayload
                                               . field "_itemMessage" _itemMessage
                                               . field "_itemTime" _itemTime
                                               . field "_itemNamespace" _itemNamespace
                                               . showString "_itemLoc = " . shows (LocShow <$> _itemLoc)
                                               . showChar '}'
                                               )
      where
        field n v = showString n . showString " = " . shows v . showString ", "

newtype LocShow = LocShow Loc


instance Show LocShow where
    showsPrec d (LocShow Loc{..}) = showParen (d >= 11) ( showString "Loc {"
                                                        . field "loc_filename" loc_filename
                                                        . field "loc_package" loc_package
                                                        . field "loc_module" loc_module
                                                        . field "loc_start" loc_start
                                                        . showString "loc_end = " . shows loc_end
                                                        . showChar '}'
                                                        )
      where
        field n v = showString n . showString " = " . shows v . showString ", "


instance ToJSON a => ToJSON (Item a) where
    toJSON Item{..} = A.object
      [ "app" A..= _itemApp
      , "env" A..= _itemEnv
      , "sev" A..= _itemSeverity
      , "thread" A..= getThreadIdText _itemThread
      , "host" A..= _itemHost
      , "pid" A..= ProcessIDJs _itemProcess
      , "data" A..= _itemPayload
      , "msg" A..= (B.toLazyText $ unLogStr _itemMessage)
      , "at" A..= _itemTime
      , "ns" A..= _itemNamespace
      , "loc" A..= fmap LocJs _itemLoc
      ]

newtype LocJs = LocJs { getLocJs :: Loc }


instance ToJSON LocJs where
    toJSON (LocJs (Loc fn p m (l, c) _)) = A.object
      [ "loc_fn" A..= fn
      , "loc_pkg" A..= p
      , "loc_mod" A..= m
      , "loc_ln" A..= l
      , "loc_col" A..= c
      ]


instance FromJSON LocJs where
    parseJSON = A.withObject "LocJs" parseLocJs
      where
        parseLocJs o = do
          fn <- o A..: "loc_fn"
          p <- o A..: "loc_pkg"
          m <- o A..: "loc_mod"
          l <- o A..: "loc_ln"
          c <- o A..: "loc_col"
          return $ LocJs $ Loc fn p m (l, c) (l, c)


instance FromJSON a => FromJSON (Item a) where
    parseJSON = A.withObject "Item" parseItem
      where
        parseItem o = Item
          <$> o A..: "app"
          <*> o A..: "env"
          <*> o A..: "sev"
          <*> o A..: "thread"
          <*> o A..: "host"
          <*> (getProcessIDJs <$> o A..: "pid")
          <*> o A..: "data"
          <*> o A..: "msg"
          <*> o A..: "at"
          <*> o A..: "ns"
          <*> (fmap getLocJs <$> o A..: "loc")


processIDToText :: ProcessID -> Text
processIDToText = toS . show


textToProcessID :: Text -> Maybe ProcessID
textToProcessID = readMay . toS


newtype ProcessIDJs = ProcessIDJs {
      getProcessIDJs :: ProcessID
    }


instance ToJSON ProcessIDJs where
    toJSON (ProcessIDJs p) = A.String (processIDToText p)


instance FromJSON ProcessIDJs where
    parseJSON = A.withText "ProcessID" parseProcessID
      where
        parseProcessID t = case textToProcessID t of
          Just p  -> return $ ProcessIDJs p
          Nothing -> fail $ "Invalid ProcessIDJs " ++ toS t


-------------------------------------------------------------------------------
-- | Field selector by verbosity within JSON payload.
data PayloadSelection
    = AllKeys
    | SomeKeys [Text]
    deriving (Show, Eq)

instance Semigroup PayloadSelection where
  AllKeys <> _ = AllKeys
  _ <> AllKeys = AllKeys
  SomeKeys as <> SomeKeys bs = SomeKeys (as <> bs)


instance Monoid PayloadSelection where
    mempty = SomeKeys []
    mappend = (<>)


-------------------------------------------------------------------------------
-- | Katip requires JSON objects to be logged as context. This
-- typeclass provides a default instance which uses ToJSON and
-- produces an empty object if 'toJSON' results in any type other than
-- object. If you have a type you want to log that produces an Array
-- or Number for example, you'll want to write an explicit instance
-- here. You can trivially add a ToObject instance for something with
-- a ToJSON instance like:
--
-- > instance ToObject Foo
class ToObject a where
    toObject :: a -> A.Object
    default toObject :: ToJSON a => a -> A.Object
    toObject v = case toJSON v of
      A.Object o -> o
      _          -> mempty

instance ToObject ()
instance ToObject A.Object

-------------------------------------------------------------------------------
-- | Payload objects need instances of this class. LogItem makes it so
-- that you can have very verbose items getting logged with lots of
-- extra fields but under normal circumstances, if your scribe is
-- configured for a lower verbosity level, it will only log a
-- selection of those keys. Furthermore, each 'Scribe' can be
-- configured with a different 'Verbosity' level. You could even use
-- 'registerScribe', 'unregisterScribe', and 'clearScribes' to at
-- runtime swap out your existing scribes for more verbose debugging
-- scribes if you wanted to.
--
-- When defining 'payloadKeys', don't redundantly declare the same
-- keys for higher levels of verbosity. Each level of verbosity
-- automatically and recursively contains all keys from the level
-- before it.
class ToObject a => LogItem a where

    -- | List of keys in the JSON object that should be included in message.
    payloadKeys :: Verbosity -> a -> PayloadSelection


instance LogItem () where payloadKeys _ _ = SomeKeys []


data AnyLogPayload = forall a. ToJSON a => AnyLogPayload a

newtype SimpleLogPayload = SimpleLogPayload {
      unSimpleLogPayload :: [(Text, AnyLogPayload)]
    }

-------------------------------------------------------------------------------
-- | A built-in convenience log payload that won't log anything on 'V0',
-- but will log everything in any other level of verbosity. Intended
-- for easy in-line usage without having to define new log types.
--
-- Construct using 'sl' and combine multiple tuples using '<>' from
-- 'Monoid'.
instance ToJSON SimpleLogPayload where
    toJSON (SimpleLogPayload as) = object $ map go as
      where go (k, AnyLogPayload v) = k A..= v


instance ToObject SimpleLogPayload


instance LogItem SimpleLogPayload where
    payloadKeys V0 _ = SomeKeys []
    payloadKeys _ _  = AllKeys


instance Semigroup SimpleLogPayload where
  SimpleLogPayload a <> SimpleLogPayload b = SimpleLogPayload (a <> b)


instance Monoid SimpleLogPayload where
    mempty = SimpleLogPayload []
    mappend = (<>)


-------------------------------------------------------------------------------
-- | Construct a simple log from any JSON item.
sl :: ToJSON a => Text -> a -> SimpleLogPayload
sl a b = SimpleLogPayload [(a, AnyLogPayload b)]


-------------------------------------------------------------------------------
-- | Constrain payload based on verbosity. Backends should use this to
-- automatically bubble higher verbosity levels to lower ones.
payloadObject :: LogItem a => Verbosity -> a -> A.Object
payloadObject verb a = case FT.foldMap (flip payloadKeys a) [(V0)..verb] of
    AllKeys     -> toObject a
    SomeKeys ks -> HM.filterWithKey (\ k _ -> k `FT.elem` ks) $ toObject a


-------------------------------------------------------------------------------
-- | Convert log item to its JSON representation while trimming its
-- payload based on the desired verbosity. Backends that push JSON
-- messages should use this to obtain their payload.
itemJson :: LogItem a => Verbosity -> Item a -> A.Value
itemJson verb a = toJSON $ a & itemPayload %~ payloadObject verb


-------------------------------------------------------------------------------
-- | Scribes are handlers of incoming items. Each registered scribe
-- knows how to push a log item somewhere.
--
-- = Guidelines for writing your own 'Scribe'
--
-- Scribes should always take a 'Severity' and 'Verbosity'.
--
-- Severity is used to __exclude log messages__ that are lower than
-- the provided Severity. For instance, if the user passes InfoS,
-- DebugS items should be ignored. Katip provides the 'permitItem'
-- utility for this. The user or the scribe may use 'permitAND' and
-- 'permitOR' to further customize this filtering, even dynamically if
-- they wish to.
--
-- Verbosity is used to select keys from the log item's payload. Each
-- 'LogItem' instance describes what keys should be retained for each
-- Verbosity level. Use the 'payloadObject' utility for extracting the keys
-- that should be written.
--
-- Scribes provide a finalizer IO action ('scribeFinalizer') that is
-- meant to synchronously flush any remaining writes and clean up any
-- resources acquired when the scribe was created. Internally, katip
-- keeps a buffer for each scribe's writes. When 'closeScribe' or
-- 'closeScribes' is called, that buffer stops accepting new log
-- messages and after the last item in its buffer is sent to 'liPush',
-- calls the finalizer. Thus, when the finalizer returns, katip can
-- assume that all resources are cleaned up and all log messages are
-- durably written.
--
-- While katip internally buffers messages per 'ScribeSettings', it
-- sends them one at a time to the scribe. Depending on the scribe
-- itself, it may make sense for that scribe to keep its own internal
-- buffer to batch-send logs if writing items one at a time is not
-- efficient. The scribe implementer must be sure that on
-- finalization, all writes are committed synchronously.

-- | Signature of a function passed to `Scribe` constructor and
--   mkScribe* functions that decides which messages to be
--   logged. Typically filters based on 'Severity', but can be
--   combined with other, custom logic with 'permitAND' and 'permitOR'
type PermitFunc = forall a. Item a -> IO Bool


-- | AND together 2 permit functions
permitAND :: PermitFunc -> PermitFunc -> PermitFunc
permitAND f1 f2 = \a -> liftA2 (&&) (f1 a) (f2 a)

-- | OR together 2 permit functions
permitOR :: PermitFunc -> PermitFunc -> PermitFunc
permitOR f1 f2 = \a -> liftA2 (||) (f1 a) (f2 a)


data Scribe = Scribe {
     liPush           :: forall a. LogItem a => Item a -> IO ()
   -- ^ How do we write an item to the scribe's output?
   , scribeFinalizer  :: IO ()
   -- ^ Provide a __blocking__ finalizer to call when your scribe is
   -- removed. All pending writes should be flushed synchronously. If
   -- this is not relevant to your scribe, return () is fine.
   , scribePermitItem :: PermitFunc
   -- ^ Provide a filtering function to allow the item to be logged,
   --   or not.  It can check Severity or some string in item's
   --   body. The initial value of this is usually created from
   --   'permitItem'. Scribes and users can customize this by ANDing
   --   or ORing onto the default with 'permitAND' or 'permitOR'
   }


whenM :: Monad m => m Bool -> m () -> m ()
whenM mbool = (>>=) mbool . flip when


-- | Combine two scribes. Publishes to the left scribe if the left
-- would permit the item and to the right scribe if the right would
-- permit the item. Finalizers are called in sequence from left to
-- right.
instance Semigroup Scribe where
  (Scribe pushA finA permitA) <> (Scribe pushB finB permitB) =
    Scribe (\item -> whenM (permitA item) (pushA item)
                  >> whenM (permitB item) (pushB item)
           )
           (finA `finally` finB)
           (permitOR permitA permitB)


instance Monoid Scribe where
    mempty = Scribe (const (return ())) (return ()) (permitItem DebugS)
    mappend = (<>)


-------------------------------------------------------------------------------
data ScribeHandle = ScribeHandle {
      shScribe :: Scribe
    , shChan   :: BQ.TBQueue WorkerMessage
    }


-------------------------------------------------------------------------------
data WorkerMessage where
  NewItem    :: LogItem a => Item a -> WorkerMessage
  PoisonPill :: WorkerMessage


-------------------------------------------------------------------------------
-- | Should this item be logged given the user's maximum severity?
-- Most new scribes will use this as a base for their 'PermitFunc'
permitItem :: Monad m => Severity -> Item a -> m Bool
permitItem sev item = return (_itemSeverity item >= sev)


-------------------------------------------------------------------------------
data LogEnv = LogEnv {
      _logEnvHost    :: HostName
    , _logEnvPid     :: ProcessID
    , _logEnvApp     :: Namespace
    -- ^ Name of application. This will typically never change. This
    -- field gets prepended to the namespace of your individual log
    -- messages. For example, if your app is MyApp and you write a log
    -- using "logItem" and the namespace "WebServer", the final
    -- namespace will be "MyApp.WebServer"
    , _logEnvEnv     :: Environment
    , _logEnvTimer   :: IO UTCTime
    -- ^ Action to fetch the timestamp. You can use something like
    -- 'AutoUpdate' for high volume logs but note that this may cause
    -- some output forms to display logs out of order. Alternatively,
    -- you could just use 'getCurrentTime'.
    , _logEnvScribes :: M.Map Text ScribeHandle
    }
makeLenses ''LogEnv


-------------------------------------------------------------------------------
-- | Create a reasonable default InitLogEnv. Uses an 'AutoUdate' which
-- updates the timer every 1ms. If you need even more timestamp
-- precision at the cost of performance, consider setting
-- '_logEnvTimer' with 'getCurrentTime'.
initLogEnv
    :: Namespace
    -- ^ A base namespace for this application
    -> Environment
    -- ^ Current run environment (e.g. @prod@ vs. @devel@)
    -> IO LogEnv
initLogEnv an env = LogEnv
  <$> getHostName
  <*> getProcessID
  <*> pure an
  <*> pure env
  <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime, updateFreq = 1000 }
  <*> pure mempty


-------------------------------------------------------------------------------
-- | Add a scribe to the list. All future log calls will go to this
-- scribe in addition to the others. Writes will be buffered per the
-- ScribeSettings to prevent slow scribes from slowing down
-- logging. Writes will be dropped if the buffer fills.
registerScribe
    :: Text
    -- ^ Name the scribe
    -> Scribe
    -> ScribeSettings
    -> LogEnv
    -> IO LogEnv
registerScribe nm scribe ScribeSettings {..} le = do
  queue <- atomically (BQ.newTBQueue (fromIntegral _scribeBufferSize))
  worker <- spawnScribeWorker scribe queue
  let fin = do
        atomically (BQ.writeTBQueue queue PoisonPill)
        -- wait for our worker to finish final write
        void (Async.waitCatch worker)
        -- wait for scribe to finish final write
        void (scribeFinalizer scribe)

  let sh = ScribeHandle (scribe { scribeFinalizer = fin }) queue
  return (le & logEnvScribes %~ M.insert nm sh)


-------------------------------------------------------------------------------
spawnScribeWorker :: Scribe -> BQ.TBQueue WorkerMessage -> IO (Async.Async ())
spawnScribeWorker (Scribe write _ _) queue = Async.async go
  where
    go = do
      newCmd <- atomically (BQ.readTBQueue queue)
      case newCmd of
        NewItem a  -> do
          -- Swallow any direct exceptions from the
          -- scribe. safe-exceptions won't catch async exceptions.
          void (tryAny (write a))
          go
        PoisonPill -> return ()


-------------------------------------------------------------------------------
data ScribeSettings = ScribeSettings {
      _scribeBufferSize :: Int
    }
  deriving (Show, Eq)

makeLenses ''ScribeSettings


-- | Reasonable defaults for a scribe. Buffer
-- size of 4096.
defaultScribeSettings :: ScribeSettings
defaultScribeSettings = ScribeSettings 4096


-------------------------------------------------------------------------------
-- | Remove a scribe from the environment. This does __not__ finalize
-- the scribe. This mainly only makes sense to use with something like
-- MonadReader's @local@ function to temporarily disavow a single
-- logger for a block of code.
unregisterScribe
    :: Text
    -- ^ Name of the scribe
    -> LogEnv
    -> LogEnv
unregisterScribe nm =  logEnvScribes %~ M.delete nm


-------------------------------------------------------------------------------
-- | Unregister __all__ scribes. Note that this is __not__ for closing or
-- finalizing scribes, use 'closeScribes' for that. This mainly only
-- makes sense to use with something like MonadReader's @local@
-- function to temporarily disavow any loggers for a block of code.
clearScribes
    :: LogEnv
    -> LogEnv
clearScribes = logEnvScribes .~ mempty


-------------------------------------------------------------------------------
-- | Finalize a scribe. The scribe is removed from the environment,
-- its finalizer is called so that it can never be written to again
-- and all pending writes are flushed. Note that this will throw any
-- exceptions yoru finalizer will throw, and that LogEnv is immutable,
-- so it will not be removed in that case.
closeScribe
    :: Text
    -- ^ Name of the scribe
    -> LogEnv
    -> IO LogEnv
closeScribe nm le = do
  maybe (return ()) (scribeFinalizer . shScribe) (M.lookup nm (_logEnvScribes le))
  return (le & logEnvScribes %~ M.delete nm)


-------------------------------------------------------------------------------
-- | Call this at the end of your program. This is a blocking call
-- that stop writing to a scribe's queue, waits for the queue to
-- empty, finalizes each scribe in the log environment and then
-- removes it. Finalizers are all run even if one of them throws, but
-- the exception will be re-thrown at the end.
closeScribes
    :: LogEnv
    -> IO LogEnv
closeScribes le = do
  -- We want to run every finalizer here so we'll not save
  -- intermediate logenvs and just clear scribes at the end.
  let actions = [void (closeScribe k le) | k <- M.keys (_logEnvScribes le)]
  FT.foldr finally (return ()) actions
  return (le & logEnvScribes .~ mempty)


-------------------------------------------------------------------------------
-- | Monads where katip logging actions can be performed. Katip is the
-- most basic logging monad. You will typically use this directly if
-- you either don't want to use namespaces/contexts heavily or if you
-- want to pass in specific contexts and/or namespaces at each log site.
--
-- For something more powerful, look at the docs for 'KatipContext',
-- which keeps a namespace and merged context. You can write simple
-- functions that add additional namespacing and merges additional
-- context on the fly.
--
-- 'localLogEnv' was added to allow for lexically-scoped modifications
-- of the log env that are reverted when the supplied monad
-- completes. 'katipNoLogging', for example, uses this to temporarily
-- pause log outputs.
class MonadIO m => Katip m where
    getLogEnv :: m LogEnv
    localLogEnv :: (LogEnv -> LogEnv) -> m a -> m a


instance Katip m => Katip (ReaderT s m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapReaderT . localLogEnv


#if !MIN_VERSION_either(4, 5, 0)
instance Katip m => Katip (EitherT s m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapEitherT . localLogEnv
#endif


instance Katip m => Katip (ExceptT s m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapExceptT . localLogEnv


instance Katip m => Katip (MaybeT m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapMaybeT . localLogEnv


instance Katip m => Katip (StateT s m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapStateT . localLogEnv


instance (Katip m, Monoid w) => Katip (RWST r w s m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapRWST . localLogEnv


instance (Katip m, Monoid w) => Katip (Strict.RWST r w s m) where
    getLogEnv = lift getLogEnv
    localLogEnv = Strict.mapRWST . localLogEnv


instance Katip m => Katip (Strict.StateT s m) where
    getLogEnv = lift getLogEnv
    localLogEnv = Strict.mapStateT . localLogEnv


instance (Katip m, Monoid s) => Katip (WriterT s m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapWriterT . localLogEnv


instance (Katip m, Monoid s) => Katip (Strict.WriterT s m) where
    getLogEnv = lift getLogEnv
    localLogEnv = Strict.mapWriterT . localLogEnv


instance (Katip m) => Katip (ResourceT m) where
    getLogEnv = lift getLogEnv
    localLogEnv = transResourceT . localLogEnv


-------------------------------------------------------------------------------
-- | A concrete monad you can use to run logging actions. Use this if
-- you prefer an explicit monad transformer stack and adding layers as
-- opposed to implementing 'Katip' for your monad.
newtype KatipT m a = KatipT { unKatipT :: ReaderT LogEnv m a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadMask, MonadCatch, MonadThrow, MonadTrans, MonadBase b)


instance MonadIO m => Katip (KatipT m) where
    getLogEnv = KatipT ask
    localLogEnv f (KatipT m) = KatipT $ local f m


instance MonadTransControl KatipT where
    type StT (KatipT) a = a
    liftWith f = KatipT $ ReaderT $ \le -> f $ \t -> runKatipT le t
    restoreT = KatipT . ReaderT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}


instance (MonadBaseControl b m) => MonadBaseControl b (KatipT m) where
  type StM ((KatipT) m) a = ComposeSt (KatipT) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadUnliftIO m => MonadUnliftIO (KatipT m) where
  askUnliftIO = KatipT $
                withUnliftIO $ \u ->
                pure (UnliftIO (unliftIO u . unKatipT))

#if MIN_VERSION_base(4, 9, 0)
instance MF.MonadFail m => MF.MonadFail (KatipT m) where
    fail msg = lift (MF.fail msg)
    {-# INLINE fail #-}
#endif

-------------------------------------------------------------------------------
-- | Execute 'KatipT' on a log env.
runKatipT :: LogEnv -> KatipT m a -> m a
runKatipT le (KatipT f) = runReaderT f le


-------------------------------------------------------------------------------
-- | Disable all scribes for the given monadic action, then restore
-- them afterwards. Works in any Katip monad.
katipNoLogging
    :: ( Katip m
       )
    => m a
    -> m a
katipNoLogging = localLogEnv (\le -> set logEnvScribes mempty le)


-------------------------------------------------------------------------------
-- | Log with everything, including a source code location. This is
-- very low level and you typically can use 'logT' in its place.
logItem
    :: (A.Applicative m, LogItem a, Katip m)
    => a
    -> Namespace
    -> Maybe Loc
    -> Severity
    -> LogStr
    -> m ()
logItem a ns loc sev msg = do
    LogEnv{..} <- getLogEnv
    liftIO $ do
      item <- Item
        <$> pure _logEnvApp
        <*> pure _logEnvEnv
        <*> pure sev
        <*> (mkThreadIdText <$> myThreadId)
        <*> pure _logEnvHost
        <*> pure _logEnvPid
        <*> pure a
        <*> pure msg
        <*> _logEnvTimer
        <*> pure (_logEnvApp <> ns)
        <*> pure loc
      FT.forM_ (M.elems _logEnvScribes) $ \ ScribeHandle {..} -> do
        whenM (scribePermitItem shScribe item) $
          void $ atomically (tryWriteTBQueue shChan (NewItem item))

-------------------------------------------------------------------------------
tryWriteTBQueue
    :: TBQueue a
    -> a
    -> STM Bool
    -- ^ Did we write?
tryWriteTBQueue q a = do
  full <- isFullTBQueue q
  unless full (writeTBQueue q a)
  return (not full)


-------------------------------------------------------------------------------
-- | Log with full context, but without any code location.
logF
  :: (Applicative m, LogItem a, Katip m)
  => a
  -- ^ Contextual payload for the log
  -> Namespace
  -- ^ Specific namespace of the message.
  -> Severity
  -- ^ Severity of the message
  -> LogStr
  -- ^ The log message
  -> m ()
logF a ns sev msg = logItem a ns Nothing sev msg



-------------------------------------------------------------------------------
-- | Perform an action while logging any exceptions that may occur.
-- Inspired by 'onException`.
--
-- >>>> logException () mempty ErrorS (error "foo")
logException
    :: (Katip m, LogItem a, MonadCatch m, Applicative m)
    => a                        -- ^ Log context
    -> Namespace                -- ^ Namespace
    -> Severity                 -- ^ Severity
    -> m b                      -- ^ Main action being run
    -> m b
logException a ns sev action = action `catchAny` \e -> f e >> throwM e
  where
    f e = logF a ns sev (msg e)
    msg e = ls (T.pack "An exception has occured: ") <> showLS e


-------------------------------------------------------------------------------
-- | Log a message without any payload/context or code location.
logMsg
    :: (Applicative m, Katip m)
    => Namespace
    -> Severity
    -> LogStr
    -> m ()
logMsg ns sev msg = logF () ns sev msg


instance TH.Lift Namespace where
    lift (Namespace xs) =
      let xs' = map T.unpack xs
      in  [| Namespace (map T.pack xs') |]


instance TH.Lift Verbosity where
    lift V0 = [| V0 |]
    lift V1 = [| V1 |]
    lift V2 = [| V2 |]
    lift V3 = [| V3 |]


instance TH.Lift Severity where
    lift DebugS     = [| DebugS |]
    lift InfoS      = [| InfoS |]
    lift NoticeS    = [| NoticeS |]
    lift WarningS   = [| WarningS |]
    lift ErrorS     = [| ErrorS |]
    lift CriticalS  = [| CriticalS |]
    lift AlertS     = [| AlertS |]
    lift EmergencyS = [| EmergencyS |]


-- | Lift a location into an Exp.
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(TH.lift a)
    $(TH.lift b)
    $(TH.lift c)
    ($(TH.lift d1), $(TH.lift d2))
    ($(TH.lift e1), $(TH.lift e2))
    |]


-------------------------------------------------------------------------------
-- | For use when you want to include location in your logs. This will
-- fill the 'Maybe Loc' gap in 'logF' of this module, and relies on implicit
-- callstacks when available (GHC > 7.8).
#if MIN_VERSION_base(4, 8, 0)
getLoc :: HasCallStack => Maybe Loc
getLoc = case getCallStack callStack of
  [] -> Nothing
  xs -> Just . toLoc . head $ filter filterKatip xs
  where
    filterKatip :: (String, SrcLoc) -> Bool
    filterKatip (_, srcloc) = not $
      "katip-" `isPrefixOf` srcLocPackage srcloc

    toLoc :: (String, SrcLoc) -> Loc
    toLoc (_, l) = Loc {
        loc_filename = srcLocFile l
      , loc_package  = srcLocPackage l
      , loc_module   = srcLocModule l
      , loc_start    = (srcLocStartLine l, srcLocStartCol l)
      , loc_end      = (srcLocEndLine   l, srcLocEndCol   l)
      }
#else
getLoc :: Maybe Loc
getLoc = Nothing
#endif


-------------------------------------------------------------------------------
-- Like `getLoc`, but uses template-haskell and works with older versions of
-- the compiler (GHC 7.8 or older).
getLocTH :: ExpQ
getLocTH = [| $(location >>= liftLoc) |]


-------------------------------------------------------------------------------
-- | 'Loc'-tagged logging when using template-haskell.
--
-- @$(logT) obj mempty InfoS "Hello world"@
logT :: ExpQ
logT = [| \ a ns sev msg -> logItem a ns (Just $(getLocTH)) sev msg |]


-------------------------------------------------------------------------------
-- | 'Loc'-tagged logging using 'GHC.Stack' when available.
--
-- This function does not require template-haskell as it
-- automatically uses <https://hackage.haskell.org/package/base-4.8.2.0/docs/GHC-Stack.html#v:getCallStack implicit-callstacks>
-- when the code is compiled using GHC > 7.8. Using an older version of the
-- compiler will result in the emission of a log line without any location information,
-- so be aware of it. Users using GHC <= 7.8 may want to use the template-haskell function
-- `logT` for maximum compatibility.
--
-- @logLoc obj mempty InfoS "Hello world"@
#if MIN_VERSION_base(4, 8, 0)
logLoc :: (Applicative m, LogItem a, Katip m, HasCallStack)
#else
logLoc :: (Applicative m, LogItem a, Katip m)
#endif
       => a
       -> Namespace
       -> Severity
       -> LogStr
       -> m ()
logLoc a ns = logItem a ns getLoc


-- taken from the file-location package
-- turn the TH Loc loaction information into a human readable string
-- leaving out the loc_end parameter
locationToString :: Loc -> String
locationToString loc = (loc_package loc) ++ ':' : (loc_module loc) ++
  ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
  where
    line = show . fst . loc_start
    char = show . snd . loc_start
