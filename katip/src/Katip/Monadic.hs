{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Provides support for treating payloads and namespaces as
-- composable contexts. The common pattern would be to provide a
-- 'KatipContext' instance for your base monad.
module Katip.Monadic
    (
    -- * Monadic variants of logging functions from "Katip.Core"
      logFM
    , logTM
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
    , KatipContextTState(..)
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Either        (EitherT)
import           Control.Monad.Trans.Except        (ExceptT)
import           Control.Monad.Trans.Identity      (IdentityT)
import           Control.Monad.Trans.List          (ListT)
import           Control.Monad.Trans.Maybe         (MaybeT)
import           Control.Monad.Trans.Resource      (ResourceT)
import           Control.Monad.Trans.RWS           (RWST)
import qualified Control.Monad.Trans.RWS.Strict    as Strict (RWST)
import qualified Control.Monad.Trans.State.Strict  as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.Foldable                     as FT
import qualified Data.HashMap.Strict               as HM
import           Data.Monoid                       as M
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
newtype LogContexts = LogContexts (Seq AnyLogContext) deriving (Monoid) --TODO: could we pre-encode this and also capture payloadKeys

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
-- | A monadic context that has an inherant way to get logging
-- context and namespace. Examples include a web application monad or
-- database monad.
class Katip m => KatipContext m where
  getKatipContext :: m LogContexts
  getKatipNamespace   :: m Namespace

--TODO: is this INLINABLE?
#define TRANS(T) \
  instance (KatipContext m, Katip (T m)) => KatipContext (T m) where \
    getKatipContext = lift getKatipContext; \
    getKatipNamespace = lift getKatipNamespace

#define TRANS_CTX(CTX, T) \
  instance (CTX, KatipContext m, Katip (T m)) => KatipContext (T m) where \
    getKatipContext = lift getKatipContext; \
    getKatipNamespace = lift getKatipNamespace

TRANS(IdentityT)
TRANS(MaybeT)
TRANS(EitherT e)
TRANS(ListT)
TRANS(ReaderT r)
TRANS(ResourceT)
TRANS(Strict.StateT s)
TRANS(StateT s)
TRANS(ExceptT s)
TRANS_CTX(Monoid w, Strict.WriterT w)
TRANS_CTX(Monoid w,        WriterT w)
TRANS_CTX(Monoid w, Strict.RWST r w s)
TRANS_CTX(Monoid w,        RWST r w s)

deriving instance (Monad m, KatipContext m) => KatipContext (KatipT m)

-------------------------------------------------------------------------------
-- | Log with everything, including a source code location. This is
-- very low level and you typically can use 'logTM' in its
-- place. Automaticallysupplies payload and namespace.
logItemM
    :: (Applicative m, KatipContext m, Katip m)
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
  :: (Applicative m, KatipContext m, Katip m)
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
-- @$(logt) InfoS "Hello world"@
logTM :: ExpQ
logTM = [| logItemM (Just $(getLoc)) |]


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
logExceptionM action sev = action `catchAll` \e -> f e >> throwM e
  where
    f e = logFM sev (msg e)
    msg e = ls ("An exception has occured: " :: Text) M.<> showLS e


-------------------------------------------------------------------------------
-- | Provides a simple transformer that defines a 'KatipContext'
-- instance for a fixed namespace and context. You will typically only
-- use this if you are forced to run in IO but still want to have your
-- log context. This is the slightly more powerful version of KatipT
-- in that it provides KatipContext instead of just Katip. For instance:
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


instance (MonadIO m) => KatipContext (KatipContextT m) where
  getKatipContext = KatipContextT $ ReaderT $ \lts -> return (ltsContext lts)
  getKatipNamespace = KatipContextT $ ReaderT $ \lts -> return (ltsNamespace lts)


-------------------------------------------------------------------------------
runKatipContextT :: (LogItem c) => LogEnv -> c -> Namespace -> KatipContextT m a -> m a
runKatipContextT le ctx ns = flip runReaderT lts . unKatipContextT
  where
    lts = KatipContextTState le (liftPayload ctx) ns
