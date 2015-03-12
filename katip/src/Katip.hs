module Katip
    (

    -- * Framework Types

      Katip (..)
    , Namespace (..)
    , Environment (..)
    , Severity (..), renderSeverity
    , Verbosity (..)
    , LogContext (..)
    , PayloadSelection (..)
    , Scribe (..)
    , LogEnv (..)

    -- * A Built-in Monad For Logging
    , KatipT (..)
    , runKatipT

    -- * Initializing Loggers
    , initLogEnv
    , registerScribe
    , unregisterScribe

    -- * Logging Functions
    , LogStr (..)
    , logStr, ls, showLS

    , logf
    , logm
    , logt
    , logi

    -- * Included Scribes
    , mkHandleScribe
    , ColorStrategy (..)
    ) where

-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------
