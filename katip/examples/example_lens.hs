{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative         as A
import           Control.Lens                hiding ((.=))
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.Monoid                 as M
import           System.IO                   (stdout)
-------------------------------------------------------------------------------
import           Katip
-------------------------------------------------------------------------------


data MyState = MyState {
    _msKNamespace :: Namespace
  , _msKContext   :: LogContexts
  , _msLogEnv     :: LogEnv
  }


-- This gives us HasMyState, which is helpful for complex stacks where
-- MyState may be nested somewhere deeper inside a larger data
-- structure. You can keep functions that operate on MyState as
-- general as possible.
makeClassy ''MyState


-------------------------------------------------------------------------------
-- | An example of advanced katip usage. Be sure to check out
-- lens_example for a slightly cleaner and more general pattern.
main :: IO ()
main = do
  le <- initLogEnv "MyApp" "production"
  -- We'll set up a scribe that logs to stdout and will only log item
  -- fields permitted for Verbosity 2 and will throw out Debug
  -- messages entirely. Note that katip provides facilities like
  -- 'unregisterScribe' and 'registerScribe' to make it possible to
  -- hot-swap scribes at runtime if you need to.
  (handleScribe, finaliser) <- mkHandleScribe ColorIfTerminal stdout InfoS V2
  let le' = registerScribe "stdout" handleScribe le
  let s = MyState M.mempty mempty le'
  runStack s $ do
    $(logTM) InfoS "Started"
    -- this will add "confrabulation" to the current namespace, making
    -- logs made under this block have the namespace of
    -- "main.confrabulation". Further, ConfrabLogCTX's key/value
    -- context will also get merged with the context above it. You can
    -- use this to stack up various contextual details throughout your
    -- code and they will be flattened out and combined in the log
    -- output.
    addNamespace "confrabulation" $ addContext (ConfrabLogCTX 42) $ do
      $(logTM) DebugS "Confrabulating widgets, with extra namespace and context"
      confrabulateWidgets
    $(logTM) InfoS "Namespace and context are back to normal"
    noLogging $
      $(logTM) DebugS "You'll never see this log message!"
  finaliser


-------------------------------------------------------------------------------
newtype ConfrabLogCTX = ConfrabLogCTX Int


instance ToJSON ConfrabLogCTX where
  toJSON (ConfrabLogCTX factor) = object ["confrab_factor" .= factor]


instance ToObject ConfrabLogCTX


instance LogItem ConfrabLogCTX where
  payloadKeys _verb _a = AllKeys


-------------------------------------------------------------------------------
confrabulateWidgets :: (Monad m) => m ()
confrabulateWidgets = return ()


-------------------------------------------------------------------------------
newtype MyStack m a = MyStack {
      unStack :: ReaderT MyState m a
    } deriving (MonadReader MyState, Functor, A.Applicative, Monad, MonadIO, MonadTrans)


-- MonadBase, MonadTransControl, and MonadBaseControl aren't strictly
-- needed for this example, but they are commonly required and
-- MonadTransControl/MonadBaseControl are a pain to implement, so I've
-- included them. Note that KatipT and KatipContextT already do this work for you.
instance MonadBase b m => MonadBase b (MyStack m) where
  liftBase = liftBaseDefault


instance MonadTransControl MyStack where
  type StT MyStack a = StT (ReaderT Int) a
  liftWith = defaultLiftWith MyStack unStack
  restoreT = defaultRestoreT MyStack


instance MonadBaseControl b m => MonadBaseControl b (MyStack m) where
  type StM (MyStack m) a = ComposeSt MyStack m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance (MonadIO m) => Katip (MyStack m) where
  getLogEnv = view msLogEnv


instance (MonadIO m) => KatipContext (MyStack m) where
  getKatipContext = view msKContext
  getKatipNamespace = view msKNamespace


-------------------------------------------------------------------------------
-- | Merge some context into the log only for the given block
addContext :: (LogItem i, MonadReader r m, HasMyState r) => i -> m a -> m a
addContext i = local (\r -> r & msKContext <>~ ctxs)
  where
    ctxs = liftPayload i


-------------------------------------------------------------------------------
-- | Add a layer of namespace to the logs only for the given block
addNamespace :: (MonadReader r m, HasMyState r) => Namespace -> m a -> m a
addNamespace ns = local (\r -> r & msKNamespace <>~ ns)


-------------------------------------------------------------------------------
-- | Disable all log output temporarily
noLogging :: (MonadReader r m, HasMyState r) => m a -> m a
noLogging = local (\r -> r & msLogEnv %~ clearScribes)


-------------------------------------------------------------------------------
runStack :: MyState -> MyStack m a -> m a
runStack s f = runReaderT (unStack f) s
