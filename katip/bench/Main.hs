{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main,
  )
where

-------------------------------------------------------------------------------
import Control.Concurrent
import Control.DeepSeq
import Control.Exception.Safe
import Control.Monad
import Criterion.Main
import Data.Aeson
import Data.Monoid as M
-------------------------------------------------------------------------------
import Katip.Core
import Katip.Scribes.Handle
import System.Directory
import System.FilePath
import System.IO
import System.Posix

-------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    [ handleScribeBench
    ]

-------------------------------------------------------------------------------
handleScribeBench :: Benchmark
handleScribeBench = bgroup "Katip.Scribes.Handle" $
  flip map destinations $ \dest ->
    bgroup
      (show dest)
      [ -- This is variably wildly on disk performance but should be a
        -- better test since a push test basically just tests how fast your
        -- queue structure is.
        bench "full env, flush 1000 writes" $
          whnfIO $ do
            le <- setupHandleLogEnv dest
            runKatipT le $ replicateM_ 1000 $ logItem ExPayload "namespace" Nothing InfoS "example"
            closeScribes le
      ]
  where
    destinations = [DevNull, TempFile]

--destinations = [DevNull]

-------------------------------------------------------------------------------
data HandleDest = DevNull | TempFile deriving (Show, Eq)

-------------------------------------------------------------------------------
instance NFData LogEnv where
  rnf (LogEnv !_ !_ !_ !_ !_ !_) = ()

setupHandleLogEnv :: HandleDest -> IO LogEnv
setupHandleLogEnv hd = do
  (scr, _) <- setupHandleEnv hd
  registerScribe "handle" scr defaultScribeSettings =<< initLogEnv "katip-bench" "bench"

-------------------------------------------------------------------------------
setupHandleEnv :: HandleDest -> IO (Scribe, ThreadIdText)
setupHandleEnv dest = do
  scribe <- setup dest
  tid <- myThreadId
  return (scribe, mkThreadIdText tid)

-------------------------------------------------------------------------------
data ExPayload = ExPayload

instance ToJSON ExPayload where
  toJSON _ = Object M.mempty

instance ToObject ExPayload

instance LogItem ExPayload where
  payloadKeys _ _ = AllKeys

-------------------------------------------------------------------------------
setup :: HandleDest -> IO Scribe
setup dest = do
  outFile <- case dest of
    TempFile -> do
      tmp <- getTemporaryDirectory
      return (tmp </> "katip-bench.log")
    DevNull -> return ("/dev/null")
  h <- openFile outFile WriteMode
  s <- mkHandleScribe ColorIfTerminal h (permitItem DebugS) V0
  let cleanupHandle = hClose h `finally` (when (dest == TempFile) (removeLink outFile))
  return s {scribeFinalizer = scribeFinalizer s `finally` cleanupHandle}

-------------------------------------------------------------------------------
deriving instance NFData ThreadIdText

instance NFData Scribe where
  rnf (Scribe a b p) =
    (a :: Item ExPayload -> IO ())
      `seq` b
      `seq` (p :: Item ExPayload -> IO Bool)
      `seq` ()
