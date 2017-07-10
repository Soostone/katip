{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative         as A
import           Control.Concurrent.STM
import           Control.Lens                hiding (mapping, (.=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import qualified Data.HashMap.Strict         as HM
import           Data.Monoid
import           Data.Scientific
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Data.Typeable
import qualified Data.Vector                 as V
import qualified Database.V1.Bloodhound      as V1
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Test.QuickCheck.Instances   ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Options
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Katip
import           Katip.Scribes.ElasticSearch
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain $ askOption $ \vers -> testGroup "katip-elasticsearch"
  [
    esTests vers
  , typeAnnotatedTests
  , roundToSundayTests
  ]


-------------------------------------------------------------------------------
data TestWithESVersion = TestV1
                       | TestV5
                       deriving (Typeable)


instance IsOption TestWithESVersion where
  defaultValue = TestV1
  parseValue "1" = Just TestV1
  parseValue "5" = Just TestV5
  parseValue _   = Nothing
  optionName = "es-version"
  optionHelp = "Version of ES to test against, either 1 or 5, defaulting to 1."


-------------------------------------------------------------------------------
setupSearch :: (EsScribeCfg ESV1 -> EsScribeCfg ESV1) -> IO Scribe
setupSearch modScribeCfg = do
    bh dropESSchema
    mgr <- newManager defaultManagerSettings
    mkEsScribe cfg (V1.mkBHEnv svr mgr) ixn mn DebugS V3
  where
    cfg = modScribeCfg (defaultEsScribeCfgV1 { essAnnotateTypes = True
                                             , essIndexSettings = ixs
                                             })


-------------------------------------------------------------------------------
teardownSearch :: IO ()
teardownSearch = do
  bh $ do
    when False $ dropESSchema
    when False $ dropESSTemplate


-------------------------------------------------------------------------------
withSearch :: (IO Scribe -> TestTree) -> TestTree
withSearch = withSearch' id


-------------------------------------------------------------------------------
withSearch' :: (EsScribeCfg ESV1 -> EsScribeCfg ESV1) -> (IO Scribe -> TestTree) -> TestTree
withSearch' modScribeCfg = withResource (setupSearch modScribeCfg) (const teardownSearch)


-------------------------------------------------------------------------------
--TODO: switch off version
esTests :: TestWithESVersion -> TestTree
esTests _ = testGroup "elasticsearch scribe"
  [
    withSearch' (\c -> c { essIndexSharding = NoIndexSharding}) $ \setup -> testCase "it flushes to elasticsearch" $ withTestLogging setup $ \done -> do
       $(logT) (ExampleCtx True) mempty InfoS "A test message"
       liftIO $ do
         void done
         logs <- getLogs
         length logs @?= 1
         let l = head logs
         l ^? key "_source" . key "msg" . _String @?= Just "A test message"
         l ^? key "_source" . key "data" . key "whatever::b" . _Bool @?= Just True
  , withSearch $ \setup -> testCase "date-based index sharding" $ do
      let t1 = mkTime 2016 1 2 3 4 5
      fakeClock <- newTVarIO t1
      withTestLogging' (set logEnvTimer (readTVarIO fakeClock)) setup $ \done -> do
        $(logT) (ExampleCtx True) mempty InfoS "today"
        let t2 = mkTime 2016 1 3 3 4 5
        liftIO (atomically (writeTVar fakeClock t2))
        $(logT) (ExampleCtx True) mempty InfoS "tomorrow"
        liftIO $ do
          void done
          todayLogs <- getLogsByIndex (V1.IndexName "katip-elasticsearch-tests-2016-01-02")
          tomorrowLogs <- getLogsByIndex (V1.IndexName "katip-elasticsearch-tests-2016-01-03")
          assertBool ("todayLogs has " <> show (length todayLogs) <> " items") (length todayLogs == 1)
          assertBool ("tomorrowLogs has " <> show (length tomorrowLogs) <> " items") (length tomorrowLogs == 1)
          let logToday = head todayLogs
          let logTomorrow = head tomorrowLogs
          logToday ^? key "_source" . key "msg" . _String @?= Just "today"
          logTomorrow ^? key "_source" . key "msg" . _String @?= Just "tomorrow"
  , withSearch' (\c -> c { essIndexSharding = WeeklyIndexSharding}) $ \setup -> testCase "weekly index sharding rounds to previous sunday" $ do
      let t1 = mkTime 2016 3 5 0 0 0 -- saturday, march 5th
      fakeClock <- newTVarIO t1
      withTestLogging' (set logEnvTimer (readTVarIO fakeClock)) setup $ \done -> do
        $(logT) (ExampleCtx True) mempty InfoS "today"
        let t2 = mkTime 2016 3 6 0 0 0 -- sunday march 6th
        liftIO (atomically (writeTVar fakeClock t2))
        $(logT) (ExampleCtx True) mempty InfoS "tomorrow"
        liftIO $ do
          void done
          todayLogs <- getLogsByIndex (V1.IndexName "katip-elasticsearch-tests-2016-02-28") -- rounds back to previous sunday
          tomorrowLogs <- getLogsByIndex (V1.IndexName "katip-elasticsearch-tests-2016-03-06") -- is on sunday, so uses current date
          assertBool ("todayLogs has " <> show (length todayLogs) <> " items") (length todayLogs == 1)
          assertBool ("tomorrowLogs has " <> show (length tomorrowLogs) <> " items") (length tomorrowLogs == 1)
          let logToday = head todayLogs
          let logTomorrow = head tomorrowLogs
          logToday ^? key "_source" . key "msg" . _String @?= Just "today"
          logTomorrow ^? key "_source" . key "msg" . _String @?= Just "tomorrow"
  ]


-------------------------------------------------------------------------------
mkTime :: Integer -> Int -> Int -> DiffTime -> DiffTime -> DiffTime -> UTCTime
mkTime y m d hr minute s = UTCTime day dt
  where
    day = mkDay y m d
    dt = s + 60 * minute + 60 * 60 * hr


-------------------------------------------------------------------------------
mkDay :: Integer -> Int -> Int -> Day
mkDay y m d = day
  where
    Just day = fromGregorianValid y m d


-------------------------------------------------------------------------------
data ExampleCtx = ExampleCtx {
      ecBool :: Bool
    }

instance ToJSON ExampleCtx where
  toJSON c = object ["whatever" .= ecBool c]


instance ToObject ExampleCtx


instance LogItem ExampleCtx where
  payloadKeys _ _ = AllKeys

-------------------------------------------------------------------------------
typeAnnotatedTests :: TestTree
typeAnnotatedTests = testGroup "TypeAnnotated"
  [
    testCase "annotates values on toJSON" $
      toJSON (TypeAnnotated exampleValue) @?= annotatedExampleValue

  , testCase "annotates values on toObject" $
      toObject (TypeAnnotated exampleObject) @?= annotatedExampleObject

  , testCase "deannotates on parseJSON" $
      parseEither parseJSON (toJSON exampleValue) @?= Right exampleValue

  , testProperty "roundtrips the same as raw" $ \(v :: Value) ->
      let res = typeAnnotatedValue
                <$> parseEither parseJSON (toJSON (TypeAnnotated v))
      in res === Right v
  ]


-------------------------------------------------------------------------------
roundToSundayTests :: TestTree
roundToSundayTests = testGroup "roundToSunday"
  [
    testProperty "always returns a sunday" $ \d ->
      getDOW (roundToSunday d) === 7
  , testProperty "returns input on sunday" $ \d -> getDOW d == 7 ==>
      roundToSunday d === d
  , testProperty "goes back a week when not sunday" $ \d -> getDOW d /= 7 ==>
      roundToSunday d < d
  ]
  where
    getDOW = view _3 . toWeekDate


-------------------------------------------------------------------------------
exampleObject :: Object
exampleObject = HM.fromList
  [ ("a bool", Bool False)
  , ("a long", Number 24)
  , ("a double", Number 52.3)
  , ("a string", String "s")
  , ("a null", Null)
  , ("a map", Object (HM.singleton "baz" (Bool True)))
  ]


-------------------------------------------------------------------------------
annotatedExampleObject :: Object
annotatedExampleObject = HM.fromList
  [ ("a map",Object $ HM.fromList [("baz::b", Bool True)])
  , ("a bool::b", Bool False)
  , ("a null::n", Null)
  , ("a string::s", String "s")
  , ("a double::d", Number 52.3)
  , ("a long::l", Number 24.0)
  ]


-------------------------------------------------------------------------------
exampleValue :: Value
exampleValue = Array $ V.fromList [Null, Object exampleObject]


-------------------------------------------------------------------------------
annotatedExampleValue :: Value
annotatedExampleValue = Array $ V.fromList
  [ Null
  , Object annotatedExampleObject
  ]


-------------------------------------------------------------------------------
getLogs :: IO [Value]
getLogs = getLogsByIndex ixn


-------------------------------------------------------------------------------
getLogsByIndex :: V1.IndexName -> IO [Value]
getLogsByIndex i = do
  r <- bh $ do
    void (V1.refreshIndex i)
    V1.searchByIndex i (V1.mkSearch Nothing Nothing)
  let actualCode = statusCode (responseStatus r)
  assertBool ("search by " <> show i <> " " <> show actualCode <> " /= 200") (actualCode == 200)
  return $ responseBody r ^.. key "hits" . key "hits" . values


-------------------------------------------------------------------------------
bh :: V1.BH IO a -> IO a
bh = V1.withBH defaultManagerSettings svr


-------------------------------------------------------------------------------
withTestLogging
  :: IO Scribe -> (IO V1.Reply -> KatipT IO b) -> IO b
withTestLogging = withTestLogging' id


-------------------------------------------------------------------------------
withTestLogging'
  :: (LogEnv -> LogEnv)
  -> IO Scribe
  -> (IO V1.Reply -> KatipT IO b)
  -> IO b
withTestLogging' modEnv setup f = do
  scr <- setup
  le <- modEnv <$> initLogEnv ns env
  le' <- registerScribe "es" scr defaultScribeSettings le
  let done' = do
        _ <- closeScribes le'
        bh (V1.refreshIndex ixn)
  runKatipT le' (f done')
  where
    ns = Namespace ["katip-test"]
    env = Environment "test"


-------------------------------------------------------------------------------
svr :: V1.Server
svr = V1.Server "http://localhost:9200"


-------------------------------------------------------------------------------
ixn :: V1.IndexName
ixn = V1.IndexName "katip-elasticsearch-tests"


-------------------------------------------------------------------------------
ixs :: V1.IndexSettings
ixs = V1.defaultIndexSettings { V1.indexShards = V1.ShardCount 1
                              , V1.indexReplicas = V1.ReplicaCount 1}

-------------------------------------------------------------------------------
mn :: V1.MappingName
mn = V1.MappingName "logs"


-------------------------------------------------------------------------------
dropESSchema :: V1.BH IO ()
dropESSchema = void $ V1.deleteIndex (V1.IndexName "katip-elasticsearch-tests*")


-------------------------------------------------------------------------------
dropESSTemplate :: V1.BH IO ()
dropESSTemplate = void $ V1.deleteTemplate (V1.TemplateName "katip-elasticsearch-tests")


-------------------------------------------------------------------------------
instance Arbitrary Value where
  arbitrary = oneof
    [ Object <$> reduceSize arbitrary
    , Array . V.fromList <$> reduceSize arbitrary
    , String <$> arbitrary
    , Number <$> (scientific <$> arbitrary <*> arbitrary)
    , Bool <$> arbitrary
    , A.pure Null
    ]


-------------------------------------------------------------------------------
-- | Reduce the size of Arbitrary input for the given generator
reduceSize :: Gen a -> Gen a
reduceSize f = sized $ \ n -> resize (n `div` 2) f
