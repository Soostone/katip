module Katip
    ( module Katip.Scribes.Handle
    , Namespace (..)
    , Environment (..)
    , Severity (..), renderSeverity
    , Verbosity (..)
    , LogContext (..)
    , PayloadSelection (..)
    , Scribe (..)
    , LogEnv (..)
    , KatipT (..)
    , runKatipT

    -- * Initializing Loggers
    , initLogEnv
    , registerScribe
    , unregisterScribe

    -- * Logging Functions
    , LogStr (..)
    , logStr, ls

    , logF
    , logM
    , logT
    , logI

    -- * Included Scribes
    , mkHandleScribe
    , ColorStrategy (..)
    ) where

-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------
