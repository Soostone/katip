{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Katip.Instances.MonadLogger
    (
    ) where


-------------------------------------------------------------------------------
import qualified Control.Applicative  as A
import qualified System.Log.FastLogger as FL
import qualified Control.Monad.Logger as L
-------------------------------------------------------------------------------
import qualified Katip                as K
-------------------------------------------------------------------------------


instance (A.Applicative m, Monad m, K.Katip m) => L.MonadLogger m where
  monadLoggerLog loc src ll msg =
    K.logItem () (K.Namespace [src]) (Just loc) (convertLL ll) (convertMsg msg)


-------------------------------------------------------------------------------
convertMsg :: (L.ToLogStr msg) => msg -> K.LogStr
convertMsg = K.ls . FL.fromLogStr . L.toLogStr


-------------------------------------------------------------------------------
convertLL :: L.LogLevel -> K.Severity
convertLL L.LevelDebug     = K.DebugS
convertLL L.LevelInfo      = K.InfoS
convertLL L.LevelWarn      = K.WarningS
convertLL L.LevelError     = K.ErrorS
convertLL (L.LevelOther _) = K.ErrorS
