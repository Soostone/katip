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
-- 'ContextualLog' instance for your base monad. When your program
-- changes to a more detailed context, say to the database, you can
-- use 'ContextualLogT' to tack on a typed context and a namespace
-- which will be merged into the enclosing monad's context and
-- namespace.
module Katip.Monadic
    (
    -- * Monadic variants of logging functions from "Katip.Core"
      logfM
    , logtM
    , logItemM

    -- * Machinery for merging typed log payloads/contexts
    , ContextualLog(..)
    , AnyLogContext
    , LogContexts
    , liftPayload

    -- * Transformer for appending to a log context
    , ContextualLogT'
    , ContextualLogT
    , runContextualLogT
    , BlankLogContextT
    , runBlankLogContextT
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Error.Class
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.RWS.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Either        (EitherT)
import           Control.Monad.Trans.Error         (ErrorT)
import           Control.Monad.Trans.Identity      (IdentityT)
import           Control.Monad.Trans.List          (ListT)
import           Control.Monad.Trans.Maybe         (MaybeT)
import           Control.Monad.Trans.Reader        (ReaderT (..), runReaderT)
import           Control.Monad.Trans.RWS           (RWST)
import qualified Control.Monad.Trans.RWS.Strict    as Strict (RWST)
import           Control.Monad.Trans.State         (StateT)
import qualified Control.Monad.Trans.State.Strict  as Strict (StateT)
import           Control.Monad.Trans.Writer        (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import           Data.Aeson
import qualified Data.HashMap.Strict               as HM
import           Data.Monoid
import           Language.Haskell.TH
-------------------------------------------------------------------------------
import           Katip.Core
-------------------------------------------------------------------------------

-- | A wrapper around a log context that erases type information so
-- that contexts from multiple layers can be combined intelligently.
data AnyLogContext where
    AnyLogContext :: (LogContext a) => a -> AnyLogContext


-------------------------------------------------------------------------------
-- | Heterogeneous list of log contexts that provides a smart
-- 'LogContext' instance for combining multiple payload policies.
newtype LogContexts = LogContexts [AnyLogContext] deriving (Monoid)

instance ToJSON LogContexts where
    toJSON (LogContexts cs) =
      Object $ mconcat $ map (\(AnyLogContext v) -> toObject v) cs

instance ToObject LogContexts

instance LogContext LogContexts where
    payloadKeys verb (LogContexts vs) = mconcat $ map payloadKeys' vs
      where
        -- To ensure AllKeys doesn't leak keys from other values when
        -- combined, we resolve AllKeys to its equivalent SomeKeys
        -- representation first.
        payloadKeys' (AnyLogContext v) = case payloadKeys verb v of
          AllKeys -> SomeKeys $ HM.keys $ toObject v
          x       -> x


-------------------------------------------------------------------------------
-- | Lift a log context into the generic wrapper
liftPayload :: (LogContext a) => a -> LogContexts
liftPayload = LogContexts . (:[]) . AnyLogContext


-------------------------------------------------------------------------------
-- | A monadic context that has an inherant way to get logging
-- context and namespace. Examples include a web application monad or
-- database monad. Combine with 'ContextualLogT' to nest.
class ContextualLog m where
  getLogContexts :: m LogContexts
  getNamespace   :: m Namespace

--TODO: is this INLINABLE?
#define TRANS(T) \
  instance (ContextualLog m, Monad m) => ContextualLog (T m) where \
    getLogContexts = lift getLogContexts; \
    getNamespace = lift getNamespace

#define TRANS_CTX(CTX, T) \
  instance (CTX, ContextualLog m, Monad m) => ContextualLog (T m) where \
    getLogContexts = lift getLogContexts; \
    getNamespace = lift getNamespace

TRANS(IdentityT)
TRANS(MaybeT)
TRANS(EitherT e)
TRANS(ListT)
TRANS(ReaderT r)
TRANS(Strict.StateT s)
TRANS(StateT s)
TRANS_CTX(Error e,         ErrorT e)
TRANS_CTX(Monoid w, Strict.WriterT w)
TRANS_CTX(Monoid w,        WriterT w)
TRANS_CTX(Monoid w, Strict.RWST r w s)
TRANS_CTX(Monoid w,        RWST r w s)

deriving instance (Monad m, ContextualLog m) => ContextualLog (KatipT m)

-------------------------------------------------------------------------------
-- | Log with everything, including a source code location. This is
-- very low level and you typically can use 'logtM' in its
-- place. Automaticallysupplies payload and namespace.
logItemM
    :: (Applicative m, ContextualLog m, Katip m)
    => Maybe Loc
    -> Severity
    -> LogStr
    -> m ()
logItemM loc sev msg = do
    ctx <- getLogContexts
    ns <- getNamespace
    logItem ctx ns loc sev msg



-------------------------------------------------------------------------------
-- | Log with full context, but without any code
-- location. Automatically supplies payload and namespace.
logfM
  :: (Applicative m, ContextualLog m, Katip m)
  => Severity
  -- ^ Severity of the message
  -> LogStr
  -- ^ The log message
  -> m ()
logfM sev msg = do
  ctx <- getLogContexts
  ns <- getNamespace
  logf ctx ns sev msg


-------------------------------------------------------------------------------
-- | 'Loc'-tagged logging when using template-haskell is OK. Automatically supplies payload and namespace.
--
-- @$(logt) InfoS "Hello world"@
logtM :: ExpQ
logtM = [| logItemM (Just $(getLoc)) |]


-------------------------------------------------------------------------------
-- | Wrapper over 'ReaderT' that provides a passthrough instance for
-- 'MonadReader' so your stack will not be affected. Also provides a
-- 'ContextualLog' instance which combines with the inner monad's
-- instance.
--
-- @
--   instance ContextualLog m WebMonad where
--     getLogContexts = ...
--     getNamespace = Namespace ["web"]
--
--   dbCallInWeb = runContextualLogT getDBContext (Namespace ["db"]) $ do
--     someStuff
-- @
--
-- In the above example, any logging inside of @dbCallInWeb@ will get
-- the (optionally) monadic log context for the db, combine it with
-- the web server's and will use the namespace @Namespace ["web","db"]@.
--
-- 'ContextualLogT' is exported which fixes @n ~ m@. The two type
-- variables must be present for defining a 'MonadTransControl' instance.
newtype ContextualLogT' n m a = ContextualLogT {
      unContextualLogT :: ReaderT (n LogContexts, n Namespace) m a
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
               , MonadFix
               , Katip
               )

type ContextualLogT m a = ContextualLogT' m m a

instance MonadTrans (ContextualLogT' n) where
    lift = ContextualLogT . lift

-- Reader is a passthrough. We don't expose our internal reader so as not to conflict
instance (MonadReader r m) => MonadReader r (ContextualLogT' n m) where
    ask = lift ask
    local f (ContextualLogT (ReaderT m)) = ContextualLogT $ ReaderT $ \r ->
      local f (m r)

instance MonadTransControl (ContextualLogT' n) where
    type StT (ContextualLogT' n) a = StT (ReaderT (n LogContexts)) a
    liftWith = defaultLiftWith ContextualLogT unContextualLogT
    restoreT = defaultRestoreT ContextualLogT
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (ContextualLogT' n m) where
  type StM ((ContextualLogT' n) m) a = ComposeSt (ContextualLogT' n) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance (ContextualLog m, Monad m) => ContextualLog (ContextualLogT' m m) where
    getLogContexts = ContextualLogT $ ReaderT $ \(gctxs, _) -> do
      ctxs <- gctxs
      ctxs' <- getLogContexts
      return $ ctxs <> ctxs'
    getNamespace = ContextualLogT $ ReaderT $ \(_, getNsInner) -> do
      nsOuter <- getNamespace
      nsInner <- getNsInner
      return $ nsOuter <> nsInner

runContextualLogT
    :: (Functor m, LogContext a)
    => m a
    -> m Namespace
    -> ContextualLogT m b
    -> m b
runContextualLogT lgtr nsgtr m = runReaderT (unContextualLogT m) (liftPayload <$> lgtr, nsgtr)


-------------------------------------------------------------------------------
-- | Type analogous to IdentityT that provides an empty log
-- context. Use this if your root monad doesn't have a LogContext instance.
newtype BlankLogContextT m a = BlankLogContextT {
      runBlankLogContextT :: m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadBase b
               , MonadReader r
               , MonadState s
               , MonadWriter w
               , MonadError e
               , MonadPlus
               , MonadFix
               , Katip
               )

instance Monad m => ContextualLog (BlankLogContextT m) where
    getLogContexts = return mempty
    getNamespace = return mempty

instance MonadTrans BlankLogContextT where
    lift = BlankLogContextT

instance MonadTransControl BlankLogContextT where
    type StT BlankLogContextT a = a
    liftWith f = BlankLogContextT $ f runBlankLogContextT
    restoreT = BlankLogContextT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (BlankLogContextT m) where
  type StM (BlankLogContextT m) a = ComposeSt BlankLogContextT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
