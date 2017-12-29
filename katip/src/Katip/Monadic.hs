{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
#if MIN_VERSION_base(4, 9, 0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
-- | Provides support for treating payloads and namespaces as
-- composable contexts. The common pattern would be to provide a
-- 'KatipContext' instance for your base monad.
module Katip.Monadic
    (
    -- * Monadic variants of logging functions from "Katip.Core"
      logFM
    , logTM
    , logLocM
    , logItemM
    , logExceptionM

    -- * Machinery for merging typed log payloads/contexts
    , KatipContext(..)
    , AnyLogContext
    , LogContexts
    , liftPayload

    -- * KatipContextT - Utility transformer that provides Katip and KatipContext instances
    , KatipContextT(..)
    , runKatipContextT
    , katipAddNamespace
    , katipAddContext
    , KatipContextTState(..)
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad.Base
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
#if !MIN_VERSION_either(4, 5, 0)
import           Control.Monad.Trans.Either        (EitherT, mapEitherT)
#endif
import           Control.Monad.Trans.Except        (ExceptT, mapExceptT)
import           Control.Monad.Trans.Identity      (IdentityT, mapIdentityT)
import           Control.Monad.Trans.List          (ListT, mapListT)
import           Control.Monad.Trans.Maybe         (MaybeT, mapMaybeT)
import           Control.Monad.Trans.Resource      (ResourceT, transResourceT)
import           Control.Monad.Trans.RWS           (RWST, mapRWST)
import qualified Control.Monad.Trans.RWS.Strict    as Strict (RWST, mapRWST)
import qualified Control.Monad.Trans.State.Strict  as Strict (StateT, mapStateT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT,
                                                              mapWriterT)
import           Control.Monad.Writer              hiding ((<>))
import           Data.Aeson
import qualified Data.Foldable                     as FT
import qualified Data.HashMap.Strict               as HM
import           Data.Semigroup                    as Semi
import           Data.Sequence                     as Seq
import           Data.Text                         (Text)
import           Language.Haskell.TH
-------------------------------------------------------------------------------
import           Katip.Core
-------------------------------------------------------------------------------

-- | A wrapper around a log context that erases type information so
-- that contexts from multiple layers can be combined intelligently.
data AnyLogContext where
    AnyLogContext :: (LogItem a) => a -> AnyLogContext


-------------------------------------------------------------------------------
-- | Heterogeneous list of log contexts that provides a smart
-- 'LogContext' instance for combining multiple payload policies. This
-- is critical for log contexts deep down in a stack to be able to
-- inject their own context without worrying about other context that
-- has already been set. Also note that contexts are treated as a
-- sequence and '<>' will be appended to the right hand side of the
-- sequence. If there are conflicting keys in the contexts, the /right
-- side will take precedence/, which is counter to how monoid works
-- for 'Map' and 'HashMap', so bear that in mind. The reasoning is
-- that if the user is /sequentially/ adding contexts to the right
-- side of the sequence, on conflict the intent is to overwrite with
-- the newer value (i.e. the rightmost value).
--
-- Additional note: you should not mappend LogContexts in any sort of
-- infinite loop, as it retains all data, so that would be a memory
-- leak.
newtype LogContexts = LogContexts (Seq AnyLogContext) deriving (Monoid, Semigroup)

instance ToJSON LogContexts where
    toJSON (LogContexts cs) =
      -- flip mappend to get right-biased merge
      Object $ FT.foldr (flip mappend) mempty $ fmap (\(AnyLogContext v) -> toObject v) cs

instance ToObject LogContexts

instance LogItem LogContexts where
    payloadKeys verb (LogContexts vs) = FT.foldr (flip mappend) mempty $ fmap payloadKeys' vs
      where
        -- To ensure AllKeys doesn't leak keys from other values when
        -- combined, we resolve AllKeys to its equivalent SomeKeys
        -- representation first.
        payloadKeys' (AnyLogContext v) = case payloadKeys verb v of
          AllKeys -> SomeKeys $ HM.keys $ toObject v
          x       -> x


-------------------------------------------------------------------------------
-- | Lift a log context into the generic wrapper so that it can
-- combine with the existing log context.
liftPayload :: (LogItem a) => a -> LogContexts
liftPayload = LogContexts . Seq.singleton . AnyLogContext


-------------------------------------------------------------------------------
-- | A monadic context that has an inherant way to get logging context
-- and namespace. Examples include a web application monad or database
-- monad. The @local@ variants are just like @local@ from Reader and
-- indeed you can easily implement them with @local@ if you happen to
-- be using a Reader in your monad. These give us 'katipAddNamespace'
-- and 'katipAddContext' that works with *any* 'KatipContext', as
-- opposed to making users have to implement these functions on their
-- own in each app.
class Katip m => KatipContext m where
  getKatipContext :: m LogContexts
  -- | Temporarily modify the current context for the duration of the
  -- supplied monad. Used in 'katipAddContext'
  localKatipContext :: (LogContexts -> LogContexts) -> m a -> m a
  getKatipNamespace :: m Namespace
  -- | Temporarily modify the current namespace for the duration of the
  -- supplied monad. Used in 'katipAddContext'
  localKatipNamespace :: (Namespace -> Namespace) -> m a -> m a

instance (KatipContext m, Katip (IdentityT m)) => KatipContext (IdentityT m) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapIdentityT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapIdentityT . localKatipNamespace


instance (KatipContext m, Katip (MaybeT m)) => KatipContext (MaybeT m) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapMaybeT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapMaybeT . localKatipNamespace


#if !MIN_VERSION_either(4, 5, 0)
instance (KatipContext m, Katip (EitherT e m)) => KatipContext (EitherT e m) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapEitherT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapEitherT . localKatipNamespace
#endif


instance (KatipContext m, Katip (ListT m)) => KatipContext (ListT m) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapListT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapListT . localKatipNamespace


instance (KatipContext m, Katip (ReaderT r m)) => KatipContext (ReaderT r m) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapReaderT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapReaderT . localKatipNamespace


instance (KatipContext m, Katip (ResourceT m)) => KatipContext (ResourceT m) where
  getKatipContext = lift getKatipContext
  localKatipContext = transResourceT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = transResourceT . localKatipNamespace


instance (KatipContext m, Katip (Strict.StateT s m)) => KatipContext (Strict.StateT s m) where
  getKatipContext = lift getKatipContext
  localKatipContext = Strict.mapStateT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = Strict.mapStateT . localKatipNamespace


instance (KatipContext m, Katip (StateT s m)) => KatipContext (StateT s m) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapStateT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapStateT . localKatipNamespace


instance (KatipContext m, Katip (ExceptT e m)) => KatipContext (ExceptT e m) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapExceptT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapExceptT . localKatipNamespace


instance (Monoid w, KatipContext m, Katip (Strict.WriterT w m)) => KatipContext (Strict.WriterT w m) where
  getKatipContext = lift getKatipContext
  localKatipContext = Strict.mapWriterT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = Strict.mapWriterT . localKatipNamespace


instance (Monoid w, KatipContext m, Katip (WriterT w m)) => KatipContext (WriterT w m) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapWriterT . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapWriterT . localKatipNamespace


instance (Monoid w, KatipContext m, Katip (Strict.RWST r w s m)) => KatipContext (Strict.RWST r w s m) where
  getKatipContext = lift getKatipContext
  localKatipContext = Strict.mapRWST . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = Strict.mapRWST . localKatipNamespace


instance (Monoid w, KatipContext m, Katip (RWST r w s m)) => KatipContext (RWST r w s m) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapRWST . localKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapRWST . localKatipNamespace


deriving instance (Monad m, KatipContext m) => KatipContext (KatipT m)

-------------------------------------------------------------------------------
-- | Log with everything, including a source code location. This is
-- very low level and you typically can use 'logTM' in its
-- place. Automatically supplies payload and namespace.
logItemM
    :: (Applicative m, KatipContext m)
    => Maybe Loc
    -> Severity
    -> LogStr
    -> m ()
logItemM loc sev msg = do
    ctx <- getKatipContext
    ns <- getKatipNamespace
    logItem ctx ns loc sev msg


-------------------------------------------------------------------------------
-- | Log with full context, but without any code
-- location. Automatically supplies payload and namespace.
logFM
  :: (Applicative m, KatipContext m)
  => Severity
  -- ^ Severity of the message
  -> LogStr
  -- ^ The log message
  -> m ()
logFM sev msg = do
  ctx <- getKatipContext
  ns <- getKatipNamespace
  logF ctx ns sev msg


-------------------------------------------------------------------------------
-- | 'Loc'-tagged logging when using template-haskell. Automatically
-- supplies payload and namespace.
--
-- @$(logTM) InfoS "Hello world"@
logTM :: ExpQ
logTM = [| logItemM (Just $(getLocTH)) |]


-------------------------------------------------------------------------------
-- | 'Loc'-tagged logging when using template-haskell. Automatically
-- supplies payload and namespace.
--
-- Same consideration as `logLoc` applies.
--
-- This function does not require template-haskell as it
-- automatically uses <https://hackage.haskell.org/package/base-4.8.2.0/docs/GHC-Stack.html#v:getCallStack implicit-callstacks>
-- when the code is compiled using GHC > 7.8. Using an older version of the
-- compiler will result in the emission of a log line without any location information,
-- so be aware of it. Users using GHC <= 7.8 may want to use the template-haskell function
-- `logTM` for maximum compatibility.
--
-- @logLocM InfoS "Hello world"@
logLocM :: (Applicative m, KatipContext m)
        => Severity
        -> LogStr
        -> m ()
logLocM = logItemM getLoc


-------------------------------------------------------------------------------
-- | Perform an action while logging any exceptions that may occur.
-- Inspired by 'onException`.
--
-- >>>> error "foo" `logExceptionM` ErrorS
logExceptionM
    :: (KatipContext m, MonadCatch m, Applicative m)
    => m a                      -- ^ Main action to run
    -> Severity                 -- ^ Severity
    -> m a
logExceptionM action sev = action `catchAny` \e -> f e >> throwM e
  where
    f e = logFM sev (msg e)
    msg e = ls ("An exception has occured: " :: Text) Semi.<> showLS e


-------------------------------------------------------------------------------
-- | Provides a simple transformer that defines a 'KatipContext'
-- instance for a fixed namespace and context. Just like 'KatipT', you
-- should use this if you prefer an explicit transformer stack and
-- don't want to (or cannot) define 'KatipContext' for your monad
-- . This is the slightly more powerful version of KatipT in that it
-- provides KatipContext instead of just Katip. For instance:
--
-- @
--   threadWithLogging = do
--     le <- getLogEnv
--     ctx <- getKatipContext
--     ns <- getKatipNamespace
--     forkIO $ runKatipContextT le ctx ns $ do
--       $(logTM) InfoS "Look, I can log in IO and retain context!"
--       doOtherStuff
-- @
newtype KatipContextT m a = KatipContextT {
      unKatipContextT :: ReaderT KatipContextTState m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadBase b
               , MonadState s
               , MonadWriter w
               , MonadError e
               , MonadPlus
               , Alternative
               , MonadFix
               , MonadTrans
               )


data KatipContextTState = KatipContextTState {
      ltsLogEnv    :: !LogEnv
    , ltsContext   :: !LogContexts
    , ltsNamespace :: !Namespace
    }


instance MonadTransControl KatipContextT where
    type StT KatipContextT a = StT (ReaderT KatipContextTState) a
    liftWith = defaultLiftWith KatipContextT unKatipContextT
    restoreT = defaultRestoreT KatipContextT
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}


instance (MonadBaseControl b m) => MonadBaseControl b (KatipContextT m) where
  type StM (KatipContextT m) a = ComposeSt KatipContextT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM


-- Reader is a passthrough. We don't expose our internal reader so as not to conflict
instance (MonadReader r m) => MonadReader r (KatipContextT m) where
    ask = lift ask
    local f (KatipContextT (ReaderT m)) = KatipContextT $ ReaderT $ \r ->
      local f (m r)


instance (MonadIO m) => Katip (KatipContextT m) where
  getLogEnv = KatipContextT $ ReaderT $ \lts -> return (ltsLogEnv lts)
  localLogEnv f (KatipContextT m) = KatipContextT (local (\s -> s { ltsLogEnv = f (ltsLogEnv s)}) m)


instance (MonadIO m) => KatipContext (KatipContextT m) where
  getKatipContext = KatipContextT $ ReaderT $ \lts -> return (ltsContext lts)
  localKatipContext f (KatipContextT m) = KatipContextT $ local (\s -> s { ltsContext = f (ltsContext s)}) m
  getKatipNamespace = KatipContextT $ ReaderT $ \lts -> return (ltsNamespace lts)
  localKatipNamespace f (KatipContextT m) = KatipContextT $ local (\s -> s { ltsNamespace = f (ltsNamespace s)}) m


-------------------------------------------------------------------------------
runKatipContextT :: (LogItem c) => LogEnv -> c -> Namespace -> KatipContextT m a -> m a
runKatipContextT le ctx ns = flip runReaderT lts . unKatipContextT
  where
    lts = KatipContextTState le (liftPayload ctx) ns


-------------------------------------------------------------------------------
-- | Append a namespace segment to the current namespace for the given
-- monadic action, then restore the previous state
-- afterwards. Works with anything implementing KatipContext.
katipAddNamespace
    :: (KatipContext m)
    => Namespace
    -> m a
    -> m a
katipAddNamespace ns = localKatipNamespace (<> ns)


-------------------------------------------------------------------------------
-- | Append some context to the current context for the given monadic
-- action, then restore the previous state afterwards. Important note:
-- be careful using this in a loop. If you're using something like
-- 'forever' or 'replicateM_' that does explicit sharing to avoid a
-- memory leak, youll be fine as it will *sequence* calls to
-- 'katipAddNamespace', so each loop will get the same context
-- added. If you instead roll your own recursion and you're recursing
-- in the action you provide, you'll instead accumulate tons of
-- redundant contexts and even if they all merge on log, they are
-- stored in a sequence and will leak memory. Works with anything
-- implementing KatipContext.
katipAddContext
    :: ( LogItem i
       , KatipContext m
       )
    => i
    -> m a
    -> m a
katipAddContext i = localKatipContext (<> (liftPayload i))
