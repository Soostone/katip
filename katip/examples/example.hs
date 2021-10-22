{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main,
  )
where

-------------------------------------------------------------------------------
import qualified Control.Applicative as A
import Control.Exception
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Monoid as M
-------------------------------------------------------------------------------
import Katip
import System.IO (stdout)

-------------------------------------------------------------------------------

-- | An example of advanced katip usage. Be sure to check out
-- example_lens.hs for a slightly cleaner and more general pattern.
main :: IO ()
main = do
  -- We'll set up a scribe that logs to stdout and will only log item
  -- fields permitted for Verbosity 2 and will throw out Debug
  -- messages entirely. Note that katip provides facilities like
  -- 'unregisterScribe' and 'registerScribe' to make it possible to
  -- hot-swap scribes at runtime if you need to. 'closeScribes' is
  -- blocking and flushes all messages out of a scribe and cleans up
  -- resources that were allocated at creation.
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    let s = MyState M.mempty mempty le
    runStack s $ do
      $(logTM) InfoS "Started"
      -- this will add "confrabulation" to the current namespace, making
      -- logs made under this block have the namespace of
      -- "main.confrabulation". Further, ConfrabLogCTX's key/value
      -- context will also get merged with the context above it. You can
      -- use this to stack up various contextual details throughout your
      -- code and they will be flattened out and combined in the log
      -- output.
      katipAddNamespace "confrabulation" $
        katipAddContext (ConfrabLogCTX 42) $ do
          $(logTM) DebugS "Confrabulating widgets, with extra namespace and context"
          confrabulateWidgets
      $(logTM) InfoS "Namespace and context are back to normal"
      katipNoLogging $
        $(logTM) DebugS "You'll never see this log message!"

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
data MyState = MyState
  { msKNamespace :: Namespace,
    msKContext :: LogContexts,
    msLogEnv :: LogEnv
  }

-------------------------------------------------------------------------------
newtype MyStack m a = MyStack
  { unStack :: ReaderT MyState m a
  }
  deriving (MonadReader MyState, Functor, A.Applicative, Monad, MonadIO, MonadTrans)

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
  getLogEnv = asks msLogEnv
  localLogEnv f (MyStack m) = MyStack (local (\s -> s {msLogEnv = f (msLogEnv s)}) m)

instance (MonadIO m) => KatipContext (MyStack m) where
  getKatipContext = asks msKContext
  localKatipContext f (MyStack m) = MyStack (local (\s -> s {msKContext = f (msKContext s)}) m)
  getKatipNamespace = asks msKNamespace
  localKatipNamespace f (MyStack m) = MyStack (local (\s -> s {msKNamespace = f (msKNamespace s)}) m)

-------------------------------------------------------------------------------
runStack :: MyState -> MyStack m a -> m a
runStack s f = runReaderT (unStack f) s
