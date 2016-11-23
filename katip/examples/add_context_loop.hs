{-# LANGUAGE OverloadedStrings #-}
-- | This executable is a test which runs addContext with
-- key-compatible log contexts in a tight loop. At time of writing, I
-- suspect this will result in a memory leak as we just naively
-- concatenate it into a sequence. If instead we eagerly merge the log
-- objects, each loop will overwrite the previous context and there
-- will be no memory leak. You can pass a numeric argument to this
-- executable to specify how many loops to do. -1 goes infinite.
--
-- If we didn't have to retain the log context and could eagerly merge
-- it, this wouldn't be an issue, but unfortunately we don't know
-- which keys to keep until log time, not context add time. Further,
-- we couldn't accumulate and compose key retention functions for the
-- same reason.
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Monad          (unless)
import qualified Data.Aeson             as A
import qualified Data.HashMap.Strict    as HM
import qualified Data.String.Conv       as SC
import qualified System.Environment     as SE
-------------------------------------------------------------------------------
import qualified Katip                  as K
-------------------------------------------------------------------------------


main :: IO ()
main = do
  args <- SE.getArgs
  le <- K.initLogEnv "add_context_loop" "test"
  K.runKatipContextT le () "loop" $ case args of
    []   -> run (Just 1000000)
    ns:_ -> let n = read ns
              in if n < 0
                    then run Nothing
                    else run (Just n)


-------------------------------------------------------------------------------
-- | Semantically, each loop adds more context on. If we were using
-- something like forever, unfoldM, etc, which is probably what we
-- actually want, it would add the same context each time,
-- non-redundantly and thus no leak.
run :: Maybe Int -> K.KatipContextT IO ()
run mlim = go 0
  where
    go n = do
      K.katipAddContext (SomeContext 1000) $ do
       K.logFM K.InfoS "loop"
       unless (done n) $ go (n + 1)
    done n = case mlim of
               Nothing -> False
               Just lim -> n >= lim


-------------------------------------------------------------------------------
data SomeContext = SomeContext Int


instance K.ToObject SomeContext where
  toObject (SomeContext sz) = HM.fromList [ SC.toS (show n) A..= () | n <- [0..sz]]


instance K.LogItem SomeContext where
  payloadKeys _ _ = K.AllKeys
