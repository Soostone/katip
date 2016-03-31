{-# LANGUAGE OverloadedStrings #-}
module Katip.Tests.Scribes.Handle
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text.IO     as T
import           System.Directory
import           System.IO
import           System.IO.Temp
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Regex.TDFA
-------------------------------------------------------------------------------
import           Katip
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Katip.Scribes.Handle"
  [
    withResource setup teardown $ \setupScribe -> testCase "logs the correct data" $ do
       (path, h, le) <- setupScribe
       runKatipT le $ logItem dummyLogItem "test" Nothing InfoS "test message"
       hClose h
       res <- readFile path
       let pat = "\\[[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2} [[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}\\]\\[katip-test.test\\]\\[Info\\]\\[.+\\]\\[[[:digit:]]+\\]\\[ThreadId [[:digit:]]+\\]\\[note.deep:some note\\] test message" :: String
       let matches = res =~ pat
       assertBool (res <> " did not match") matches
  ]


-------------------------------------------------------------------------------
data DummyLogItem = DummyLogItem {
      dliNote :: Text
    }


instance ToJSON DummyLogItem where
  toJSON (DummyLogItem n) = object
    [ "note" .= object [ "deep" .= n
                       ]
    ]


instance ToObject DummyLogItem


instance LogItem DummyLogItem where
  payloadKeys _ _ = AllKeys


-------------------------------------------------------------------------------
dummyLogItem :: DummyLogItem
dummyLogItem = DummyLogItem "some note"


-------------------------------------------------------------------------------
setup :: IO (FilePath, Handle, LogEnv)
setup = do
  tempDir <- getTemporaryDirectory
  (fp, h) <- openTempFile tempDir "katip.log"
  s <- mkHandleScribe (ColorLog False) h DebugS V3
  le <- initLogEnv "katip-test" "test"
  return (fp, h, registerScribe "handle" s le)


-------------------------------------------------------------------------------
teardown :: (FilePath, Handle, LogEnv) -> IO ()
teardown (_, h, _) = do
  chk <- hIsOpen h
  when chk $ hClose h
