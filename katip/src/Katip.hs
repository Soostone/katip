-- | Includes all of the API's youll need to use Katip. Be sure to
-- check out the included @examples@ directory for an example of
-- usage.
--
-- To get up and running, the workflow is generally:
--
-- * Set up a 'LogEnv' using 'initLogEnv'.
--
-- * Add 'Scribe's using 'registerScribe'.
--
-- * Either use 'KatipT' or 'KatipContextT' for a pre-built
-- transformer stack or add 'Katip' and 'KatipContext' instances to
-- your own transformer stack. If you do the latter, you may want to
-- look in the @examples@ dir for some tips on composing contexts and
-- namespaces.
--
-- * Define some structured log data throughout your application and
-- implement 'ToObject' and 'LogItem' for them.
--
-- * Begin logging with 'logT', 'logTM', etc.
module Katip
    (

    -- * Framework Types

      Katip (..)
    , Namespace (..)
    , Environment (..)
    , Severity (..)
    , renderSeverity
    , severityText
    , Verbosity (..)
    , ToObject (..)
    , LogItem (..)
    , Item(..)
    , itemApp
    , itemEnv
    , itemSeverity
    , itemThread
    , itemHost
    , itemProcess
    , itemPayload
    , itemMessage
    , itemTime
    , itemNamespace
    , itemLoc
    , ThreadIdText(..)
    , PayloadSelection (..)
    , Scribe (..)
    , LogEnv (..)
    , logEnvHost
    , logEnvPid
    , logEnvNs
    , logEnvEnv
    , logEnvTimer
    , logEnvScribes
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
    , clearScribes

    -- * Logging Functions
    , LogStr (..)
    , logStr, ls, showLS

    , logF
    , logMsg
    , logT
    , logItem
    , logException
    , logFM
    , logTM
    , logItemM
    , logExceptionM

    -- * Included Scribes
    , mkHandleScribe
    , ColorStrategy (..)

    -- * KatipContextT - Utility transformer that provides Katip and KatipContext instances
    , KatipContextT
    , runKatipContextT
    ) where

-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Monadic
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------
