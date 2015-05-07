module Katip
    (

    -- * Framework Types

      Katip (..)
    , Namespace (..)
    , Environment (..)
    , Severity (..), renderSeverity
    , Verbosity (..)
    , ToObject (..)
    , LogItem (..)
    , Item(..)
    , ThreadIdText(..)
    , PayloadSelection (..)
    , Scribe (..)
    , LogEnv (..)
    , KatipContext(..)
    , AnyLogContext
    , LogContexts, liftPayload
    , SimpleLogPayload, sl

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

    , logF
    , logMsg
    , logT
    , logItem
    , logFM
    , logTM
    , logItemM

    -- * Transformer for appending to a log context
    , LogT
    , runLogT
    , BlankLogT
    , runBlankLogT

    -- * Included Scribes
    , mkHandleScribe
    , ColorStrategy (..)
    ) where

-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Monadic
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------
