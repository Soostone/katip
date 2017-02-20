{-# LANGUAGE OverloadedStrings #-}
module Katip.Tests.Scribes.Handle
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Char8      as B
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as LT
import           Data.Time
import           Language.Haskell.TH.Syntax (Loc (..))
import           Lens.Micro                 ((.~))
import           System.Directory
import           System.IO
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Text.Regex.TDFA
-------------------------------------------------------------------------------
import           Katip
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Katip.Scribes.Handle"
  [
    withResource setup teardown $ \setupScribe -> testCase "logs the correct data" $ do
       (path, h, fin, le) <- setupScribe
       runKatipT le $ logItem dummyLogItem "test" Nothing InfoS "test message"
       fin
       hClose h
       res <- readFile path
       let pat = "\\[[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2} [[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}\\]\\[katip-test.test\\]\\[Info\\]\\[.+\\]\\[[[:digit:]]+\\]\\[ThreadId [[:digit:]]+\\]\\[note.deep:some note\\] test message" :: String
       let matches = res =~ pat
       assertBool (res <> " did not match") matches
  , withResource setupTempFile teardownTempFile $ \setupFn ->
       goldenVsString "Text-golden"
                      "test/Katip/Tests/Scribes/Handle-text.golden"
                      (setupFn >>= writeTextLog)
  ]


-------------------------------------------------------------------------------
data DummyLogItem = DummyLogItem {
      dliNote :: Text
    }


instance ToJSON DummyLogItem where
  toJSON dli = object
    [ "note" .= object [ "deep" .= dliNote dli
                       ]
    ]


instance ToObject DummyLogItem


instance LogItem DummyLogItem where
  payloadKeys _ _ = AllKeys


-------------------------------------------------------------------------------
dummyLogItem :: DummyLogItem
dummyLogItem = DummyLogItem "some note"


-------------------------------------------------------------------------------
setup :: IO (FilePath, Handle, IO (), LogEnv)
setup = do
  tempDir <- getTemporaryDirectory
  (fp, h) <- openTempFile tempDir "katip.log"
  (s, finaliser) <- mkHandleScribe (ColorLog False) h DebugS V3
  le <- initLogEnv "katip-test" "test"
  return (fp, h, finaliser, registerScribe "handle" s le)


-------------------------------------------------------------------------------
teardown :: (FilePath, Handle, IO (), LogEnv) -> IO ()
teardown (_, h, _, _) = do
  chk <- hIsOpen h
  when chk $ hClose h



-- Following code tests Handle scribe output against a golden file.
-- This test will fail on non utf8 locales because golden file is in utf-8.
-- It generates all meaningfull variations of Item, and also tests
-- writing of payload of different Aeson constructors
--
-- Note: currently Handle scribe does not write Array items at all
-------------------------------------------------------------------------------
data AllTypesLogItem = AllTypesLogItem
    { atlText  :: Text
    , atlNum   :: Int
    , atlFloat :: Float
    , atlList  :: [Text]
    , atlSub   :: Maybe DummyLogItem
    }


instance ToJSON AllTypesLogItem where
  toJSON it = object
    [ "text"     .= atlText it
    , "num"      .= atlNum it
    , "float"    .= atlFloat it
    , "list"     .= atlList it
    , "sub"      .= atlSub it
    ]


instance ToObject AllTypesLogItem


instance LogItem AllTypesLogItem where
  payloadKeys _ _ = AllKeys


-------------------------------------------------------------------------------
theItem :: Item DummyLogItem
theItem = Item (Namespace ["app"])
               (Environment "production")
               (InfoS)
               (ThreadIdText "1337")
               "example"
               7331
               dummyLogItem
               "message"
               (mkUTCTime 2016 6 12 12 34 56)
               (Namespace ["foo"])
               Nothing

genItems :: [Item DummyLogItem]
genItems = concat $
  [ [ itemSeverity .~ s $ theItem
          | s <- [minBound .. maxBound]
    ]
  , [ itemThread .~ (ThreadIdText . T.pack $ show t) $ theItem
          | t <- [0 :: Int, 1, 1337, 2147483647]
    ]
  , [ itemHost .~ h $ theItem
          | h <- ["example", "www.example.com", "127.0.0.1"]
    ]
  , [ itemProcess .~ p $ theItem
          | p <- [0, 1, 1337, 2147483647]
    ]
  , [ itemMessage .~ LogStr m $ theItem
          | m <- [ "message"
                 , "message\nwith newline"
                 , LT.fromLazyText (LT.replicate 40 " a really long message")
                 , "сообщение"
                 , "哈囉世界"
                 ]
    ]
  , [ itemTime .~ t $ theItem
          | t <- genDates
    ]
  , [ itemNamespace .~ Namespace ns $ theItem
          | ns <- [ ["foo"]
                  , ["foo", "bar"]
                  , ["фу", "бар"]
                  , ["with\nnewline"] ]
    ]
  , [ itemLoc .~ l $ theItem | l <- genLocs
    ]
  ]

genDates :: [UTCTime]
genDates =
  [ mkUTCTime 2000 1   1 0   0  0.0
  , mkUTCTime 2123 12 31 23  59 59.999999999999
  , mkUTCTime 2016 10 10 1   1  5.0
  , mkUTCTime 2100 12 31 12  59 10.1
  , mkUTCTime 1982 1  1  12  30 0.000000000001
  ]

genLocs :: [Maybe Loc]
genLocs =
  [ Nothing
  , Just $ Loc "path/Some/Module.hs"
               "main"
               "Some.Module"
               (30,1) (30,14)
  , Just $ Loc "путь/Some/Module.hs"
               "main"
               "Some.Module"
               (3000,9000) (4000,1)
  ]

genTypedItems :: [Item AllTypesLogItem]
genTypedItems =
  [ itemPayload .~ p $ theItem
        | p <- [ AllTypesLogItem "" 0 0.0 [] Nothing
               , AllTypesLogItem "note" 10 5.5 ["one", "two", "three"]
                     (Just dummyLogItem)
               ]
  ]

mkUTCTime :: Integer -> Int -> Int -> DiffTime -> DiffTime -> DiffTime -> UTCTime
mkUTCTime y mt d h mn s = UTCTime day dt
  where
    day = fromGregorian y mt d
    dt = h * 60 * 60 + mn * 60 + s

-------------------------------------------------------------------------------
writeTextLog :: (FilePath, Handle) -> IO (BL.ByteString)
writeTextLog (path, h) = do
    mapM_ (put . formatOne) genItems
    mapM_ (put . formatOne) genTypedItems
    hClose h
    BL.readFile path
  where
    formatOne :: LogItem a => Item a -> Text
    formatOne = LT.toStrict . LT.toLazyText . formatItem False V3
    put = B.hPutStrLn h . T.encodeUtf8

setupTempFile :: IO (FilePath, Handle)
setupTempFile = do
    tempDir <- getTemporaryDirectory
    (fp, h) <- openBinaryTempFile tempDir "katip.log"
    return (fp, h)

teardownTempFile :: (FilePath, Handle) -> IO ()
teardownTempFile (_, h) = do
    chk <- hIsOpen h
    when chk $ hClose h
