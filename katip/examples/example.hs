{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Monoid          as M
import           System.IO            (stdout)
-------------------------------------------------------------------------------
import           Katip
-------------------------------------------------------------------------------


-- | An example of advanced katip usage. Be sure to check out
-- lens_example for a slightly cleaner and more general pattern.
main :: IO ()
main = do
  le <- initLogEnv "main" "production"
  -- We'll set up a scribe that logs to stdout and will only log item
  -- fields permitted for Verbosity 2 and will throw out Debug
  -- messages entirely. Note that katip provides facilities like
  -- 'unregisterScribe' and 'registerScribe' to make it possible to
  -- hot-swap scribes at runtime if you need to.
  handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
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
data MyState = MyState {
    msKNamespace :: Namespace
  , msKContext   :: LogContexts
  , msLogEnv     :: LogEnv
  }


-------------------------------------------------------------------------------
newtype MyStack m a = MyStack {
      unStack :: ReaderT MyState m a
    } deriving (MonadReader MyState, Functor, Applicative, Monad, MonadIO)


instance (MonadIO m) => Katip (MyStack m) where
  getLogEnv = asks msLogEnv


instance (MonadIO m) => KatipContext (MyStack m) where
  getKatipContext = asks msKContext
  getKatipNamespace = asks msKNamespace


-------------------------------------------------------------------------------
-- | Merge some context into the log only for the given block
addContext :: (LogItem i, MonadReader MyState m) => i -> m a -> m a
addContext i = local (\r -> r { msKContext = msKContext r <> ctxs })
  where
    ctxs = liftPayload i


-------------------------------------------------------------------------------
-- | Add a layer of namespace to the logs only for the given block
addNamespace :: (MonadReader MyState m) => Namespace -> m a -> m a
addNamespace ns = local (\r -> r { msKNamespace = msKNamespace r <> ns })


-------------------------------------------------------------------------------
runStack :: MyState -> MyStack m a -> m a
runStack s f = runReaderT (unStack f) s
