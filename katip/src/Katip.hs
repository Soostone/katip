-- | Includes all of the APIs youll need to use Katip. Be sure to
-- check out the included @examples@ directory for an example of
-- usage.
--
-- Here's a basic example:
--
-- @
--
-- import Control.Exception
-- import Katip
--
-- main :: IO ()
-- main = do
--   handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
--   let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
--   -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
--   bracket makeLogEnv closeScribes $ \le -> do
--     let initialContext = () -- this context will be attached to every log in your app and merged w/ subsequent contexts
--     let initialNamespace = "main"
--     runKatipContextT le initialContext initialNamespace $ do
--       $(logTM) InfoS "Hello Katip"
--       -- This adds a namespace to the current namespace and merges a piece of contextual data into your context
--       katipAddNamespace "additional_namespace" $ katipAddContext (sl "some_context" True) $ do
--         $(logTM) WarningS "Now we're getting fancy"
--       katipNoLogging $ do
--         $(logTM) DebugS "You will never see this!"
--
-- @
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
--
-- * Define your own 'Scribe' if you need to output to some as-yet
-- unsupported format or service. If you think it would be useful to
-- others, consider releasing your own package.
module Katip
    (

    -- * Framework Types
      Namespace (..)
    , Environment (..)
    , Severity (..)
    , renderSeverity
    , textToSeverity
    , Verbosity (..)
    , ToObject (..)
    , LogItem (..)
    , Item(..)
    , ThreadIdText(..)
    , PayloadSelection (..)
    , Scribe (..)
    , LogEnv (..)
    , SimpleLogPayload, sl
    , defaultScribeSettings
    , ScribeSettings
    , scribeBufferSize
    , _scribeBufferSize

    -- ** @lens@-compatible Lenses
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
    , logEnvHost
    , logEnvPid
    , logEnvApp
    , logEnvEnv
    , logEnvTimer
    , logEnvScribes

    -- * A Built-in Monad For Simple Logging
    , KatipT (..)
    , runKatipT

    -- * Initializing Loggers
    , initLogEnv
    , registerScribe
    -- * Dropping scribes temporarily
    , unregisterScribe
    , clearScribes
    -- * Finalizing scribes at shutdown
    , closeScribes
    , closeScribe

    -- * Logging Functions
    , LogStr (..)
    , logStr, ls, showLS

    -- ** 'Katip' LoggingFunctions
    -- $katiplogging
    , Katip (..)
    , logF
    , logMsg
    , logT
    , logItem
    , logException
    -- ** 'KatipContext': Logging With Context
    -- $katipcontextlogging
    , KatipContext (..)
    , logFM
    , logTM
    , logItemM
    , logExceptionM
    , AnyLogContext
    , LogContexts, liftPayload
    -- *** Temporarily Changing Logging Behavior
    , katipAddNamespace
    , katipAddContext
    , katipNoLogging

    -- * Included Scribes
    , mkHandleScribe
    , ColorStrategy (..)

    -- * Tools for implementing Scribes
    , permitItem
    , payloadObject
    , itemJson

    -- * KatipContextT - Utility transformer that provides Katip and KatipContext instances
    , KatipContextT
    , runKatipContextT
    ) where

-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Monadic
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------


{- $katiplogging

   These logging functions use the basic 'Katip' constraint and thus
will require varying degrees of explicit detail such as 'Namespace'
and individual log items to be passed in. These can be described as
the primitives of Katip logging. If you find yourself making multiple
log statements within a logical logging context for your app, you may
want to look into the 'KatipContext' family of logging functions like
'logFM' and 'logTM'. 'KatipContext' in most applications should be
considered the default. Here's an example of the pain point:

@
doDatabaseThings = do
  connId <- getConnectionId
  logF (ConnectionIDContext connId) "database" InfoS "Doing database stuff"
  \-\- ...
  logF (ConnectionIDContext connId) "database" InfoS "Wow, passing in the same context is getting tedious"
@

Another pain point to look out for is nesting actions that log in
eachother. Let's say you were writing a web app. You want to capture
some detail such as the user's ID in the logs, but you also want that
info to show up in doDatabaseThings' logs so you can associate those
two pieces of information:


@
webRequestHandler = do
  uid <- getUserId
  logF (UserIDContext uid) "web" InfoS "Starting web request"
  doDatabaseThings
@

In the above example, doDatabaseThings would overwrite that
UserIDContext with its own context and namespace. Sometimes this is
what you want and that's why 'logF' and other functions which only
require 'Katip' exist. If you are interested in combining log
contexts and namespaces, see 'KatipContext'.
-}


{- $katipcontextlogging

  These logging functions use the 'KatipContext' constraint which is a
superclass of 'Katip' that also has a mechanism for keeping track of
the current context and namespace. This means a few things:

1. Functions that use 'KatipContext' like 'logFM' and 'logTM' do not
require you to pass in 'LogItem's or 'Namespaces', they pull them from
the monadic environment.

2. It becomes easy to add functions which add namespaces and/or
contexts to the current stack of them. You can (and should) make that
action scoped to a monadic action so that when it finishes, the
previous context and namespace will be automatically restored.


'KatipContextT' provides a simple, 'ReaderT'-based implementation of
the 'KatipContext' typeclass, and provides 'katipAddContext' and
'katipAddNamespace' functions to append to the context for the
duration of a block:


@
main = do
  le <- initLogEnv "MyApp" "production"
  \-\- set up scribes here
  runKatipContext le () "main" $ do
    katipAddNamespace "nextlevel" $ do
      $(logTM) InfoS "Logs here will have namespace MyApp.main.nextlevel"

    katipAddContext TrivialContext $ do
      $(logTM) InfoS "Logs here will have context from TrivialContext"

      katipAddContext AnotherContext $ do
        $(logTM) InfoS "Logs here will have context from TrivialContext *merged with* context from AnotherContext!"

    $(logTM) InfoS "Log context restored to () and namespace to MyApp.main"
@

'katipAddNamespace' and 'katipAddContext' are one-liners, implemented
in terms of 'local' from 'MonadReader'. If you have a custom monad
transformer stack and want to add your own version of these, check out
<https://github.com/Soostone/katip/tree/master/katip/examples these
examples>.
-}
