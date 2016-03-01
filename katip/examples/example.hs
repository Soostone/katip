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
-------------------------------------------------------------------------------
import           Katip
-------------------------------------------------------------------------------


-- | An example of advanced katip usage. Be sure to check out
-- lens_example for a slightly cleaner and more general pattern.
main :: IO ()
main = do
  s <- MyState M.mempty mempty <$> initLogEnv mempty (Environment "production")
  runStack s $ do
    $(logTM) InfoS "Started"
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
