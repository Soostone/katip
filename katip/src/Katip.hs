module Katip
    (

    -- * Framework Types

      Katip (..)
    , Namespace (..)
    , Environment (..)
    , Severity (..), renderSeverity
    , Verbosity (..)
    , ToObject (..)
    , LogContext (..)
    , PayloadSelection (..)
    , Scribe (..)
    , LogEnv (..)
    , ContextualLog(..)
    , AnyLogContext
    , LogContexts, liftPayload

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
    , logItem
    , logfM
    , logtM
    , logItemM

    -- * Transformer for appending to a log context
    , ContextualLogT
    , runContextualLogT

    -- * Included Scribes
    , mkHandleScribe
    , ColorStrategy (..)
    ) where

-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Monadic
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------
