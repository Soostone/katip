{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative                     as A
import           Control.Concurrent.STM
import           Control.Lens                            hiding (mapping, (.=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.ByteString.Lazy                    (ByteString)
import qualified Data.HashMap.Strict                     as HM
import           Data.Monoid
import           Data.Scientific
import           Data.Tagged
import           Data.Text                               (Text)
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Data.Typeable                           as Typeable
import qualified Data.Vector                             as V
import qualified Database.V1.Bloodhound                  as V1
import qualified Database.V5.Bloodhound                  as V5
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Test.QuickCheck.Instances               ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Options
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Katip
import           Katip.Scribes.ElasticSearch.Annotations
import           Katip.Scribes.ElasticSearch.Internal
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMainWithIngredients ings $ askOption $ \vers -> testGroup "katip-elasticsearch"
  [
    case vers of
      TestV1 -> esTests (Typeable.Proxy :: Typeable.Proxy ESV1)
      TestV5 -> esTests (Typeable.Proxy :: Typeable.Proxy ESV5)
  , typeAnnotatedTests
  , roundToSundayTests
  ]
  where
    ings = (includingOptions [Option (Typeable.Proxy :: Typeable.Proxy TestWithESVersion)]):defaultIngredients


-------------------------------------------------------------------------------
data TestWithESVersion = TestV1
                       | TestV5
                       deriving (Typeable)


instance IsOption TestWithESVersion where
  defaultValue = TestV1
  parseValue "1" = Just TestV1
  parseValue "5" = Just TestV5
  parseValue _   = Nothing
  optionName = Tagged "es-version"
  optionHelp = Tagged "Version of ES to test against, either 1 or 5, defaulting to 1."


class ESVersion v => TestESVersion v where
  type Server v
  toServer :: proxy v -> Text -> Server v
  toMappingName :: proxy v -> Text -> MappingName v
  type Search v
  type Query v
  type Filter v
  mkSearch :: proxy v -> Maybe (Query v) -> Maybe (Filter v) -> Search v
  mkBHEnv :: proxy v -> Server v -> Manager -> BHEnv v
  type ShardCount v
  toShardCount :: proxy v -> Int -> ShardCount v
  type ReplicaCount v
  toReplicaCount :: proxy v -> Int -> ReplicaCount v
  indexShards :: proxy v -> Lens' (IndexSettings v) (ShardCount v)
  indexReplicas :: proxy v -> Lens' (IndexSettings v) (ReplicaCount v)

  deleteIndex :: proxy v -> IndexName v -> BH v IO (Response ByteString)
  deleteTemplate :: proxy v -> TemplateName v -> BH v IO (Response ByteString)
  refreshIndex :: proxy v -> IndexName v -> BH v IO (Response ByteString)
  withBH :: proxy v -> ManagerSettings -> Server v -> BH v IO a -> IO a
  searchByIndex :: proxy v -> IndexName v -> Search v -> BH v IO (Response ByteString)


instance TestESVersion ESV1 where
  type Server ESV1 = V1.Server
  toServer _ = V1.Server
  toMappingName _ = V1.MappingName
  type Search ESV1 = V1.Search
  type Query ESV1 = V1.Query
  type Filter ESV1 = V1.Filter
  type ShardCount ESV1 = V1.ShardCount
  toShardCount _ = V1.ShardCount
  type ReplicaCount ESV1 = V1.ReplicaCount
  toReplicaCount _ = V1.ReplicaCount
  mkSearch _ = V1.mkSearch
  mkBHEnv _ = V1.mkBHEnv
  indexShards _ = lens V1.indexShards (\s v -> s { V1.indexShards = v})
  indexReplicas _ = lens V1.indexReplicas (\r v -> r { V1.indexReplicas = v})

  deleteIndex _ = V1.deleteIndex
  deleteTemplate _ = V1.deleteTemplate
  refreshIndex _ = V1.refreshIndex
  withBH _ = V1.withBH
  searchByIndex _ = V1.searchByIndex


instance TestESVersion ESV5 where
  type Server ESV5 = V5.Server
  toServer _ = V5.Server
  toMappingName _ = V5.MappingName
  type Search ESV5 = V5.Search
  type Query ESV5 = V5.Query
  type Filter ESV5 = V5.Filter
  type ShardCount ESV5 = V5.ShardCount
  toShardCount _ = V5.ShardCount
  type ReplicaCount ESV5 = V5.ReplicaCount
  toReplicaCount _ = V5.ReplicaCount
  mkSearch _ = V5.mkSearch
  mkBHEnv _ = V5.mkBHEnv
  indexShards _ = lens V5.indexShards (\s v -> s { V5.indexShards = v})
  indexReplicas _ = lens V5.indexReplicas (\r v -> r { V5.indexReplicas = v})

  deleteIndex _ = V5.deleteIndex
  deleteTemplate _ = V5.deleteTemplate
  refreshIndex _ = V5.refreshIndex
  withBH _ = V5.withBH
  searchByIndex _ = V5.searchByIndex


-------------------------------------------------------------------------------
setupSearch
  :: forall proxy v. ( TestESVersion v
                     , MonadIO (BH v IO)
                     , Functor (BH v IO)
                     )
  => proxy v
  -> (EsScribeCfg v -> EsScribeCfg v)
  -> IO Scribe
setupSearch prx modScribeCfg = do
    bh prx (dropESSchema prx)
    mgr <- newManager defaultManagerSettings
    mkEsScribe cfg (mkBHEnv prx (svr prx) mgr) (ixn prx) (mn prx) DebugS V3
  where
    cfg :: EsScribeCfg v
    cfg = modScribeCfg $
      (defaultEsScribeCfg' prx)
        { essAnnotateTypes = True
        , essIndexSettings = ixs prx
        }


-------------------------------------------------------------------------------
teardownSearch
  :: ( TestESVersion v
     , Monad (BH v IO)
     , Functor (BH v IO)
     )
  => proxy v
  -> IO ()
teardownSearch prx = do
  bh prx $ do
    dropESSchema prx
    dropESSTemplate prx


-------------------------------------------------------------------------------
withSearch
  :: ( TestESVersion v
     , MonadIO (BH v IO)
     , Functor (BH v IO)
     )
  => proxy v
  -> (IO Scribe -> TestTree)
  -> TestTree
withSearch = withSearch' id


-------------------------------------------------------------------------------
withSearch'
  :: ( TestESVersion v
     , MonadIO (BH v IO)
     , Functor (BH v IO)
     )
  => (EsScribeCfg v -> EsScribeCfg v)
  -> proxy v
  -> (IO Scribe -> TestTree)
  -> TestTree
withSearch' modScribeCfg prx = withResource (setupSearch prx modScribeCfg) (const (teardownSearch prx))


-------------------------------------------------------------------------------
esTests
  :: ( TestESVersion v
     , MonadIO (BH v IO)
     , Functor (BH v IO)
     , Show (IndexName v)
     )
  => proxy v
  -> TestTree
esTests prx = testGroup "elasticsearch scribe"
  [
    withSearch' (\c -> c { essIndexSharding = NoIndexSharding}) prx $ \setup -> testCase "it flushes to elasticsearch" $ withTestLogging prx setup $ \done -> do
       $(logT) (ExampleCtx True) mempty InfoS "A test message"
       liftIO $ do
         void done
         logs <- getLogs prx
         length logs @?= 1
         let l = head logs
         l ^? key "_source" . key "msg" . _String @?= Just "A test message"
         l ^? key "_source" . key "data" . key "whatever::b" . _Bool @?= Just True
  , withSearch prx $ \setup -> testCase "date-based index sharding" $ do
      let t1 = mkTime 2016 1 2 3 4 5
      fakeClock <- newTVarIO t1
      withTestLogging' (set logEnvTimer (readTVarIO fakeClock)) prx setup $ \done -> do
        $(logT) (ExampleCtx True) mempty InfoS "today"
        let t2 = mkTime 2016 1 3 3 4 5
        liftIO (atomically (writeTVar fakeClock t2))
        $(logT) (ExampleCtx True) mempty InfoS "tomorrow"
        liftIO $ do
          void done
          todayLogs <- getLogsByIndex prx (toIndexName prx "katip-elasticsearch-tests-2016-01-02")
          tomorrowLogs <- getLogsByIndex prx (toIndexName prx "katip-elasticsearch-tests-2016-01-03")
          assertBool ("todayLogs has " <> show (length todayLogs) <> " items") (length todayLogs == 1)
          assertBool ("tomorrowLogs has " <> show (length tomorrowLogs) <> " items") (length tomorrowLogs == 1)
          let logToday = head todayLogs
          let logTomorrow = head tomorrowLogs
          logToday ^? key "_source" . key "msg" . _String @?= Just "today"
          logTomorrow ^? key "_source" . key "msg" . _String @?= Just "tomorrow"
  , withSearch' (\c -> c { essIndexSharding = WeeklyIndexSharding}) prx $ \setup -> testCase "weekly index sharding rounds to previous sunday" $ do
      let t1 = mkTime 2016 3 5 0 0 0 -- saturday, march 5th
      fakeClock <- newTVarIO t1
      withTestLogging' (set logEnvTimer (readTVarIO fakeClock)) prx setup $ \done -> do
        $(logT) (ExampleCtx True) mempty InfoS "today"
        let t2 = mkTime 2016 3 6 0 0 0 -- sunday march 6th
        liftIO (atomically (writeTVar fakeClock t2))
        $(logT) (ExampleCtx True) mempty InfoS "tomorrow"
        liftIO $ do
          void done
          todayLogs <- getLogsByIndex prx (toIndexName prx "katip-elasticsearch-tests-2016-02-28") -- rounds back to previous sunday
          tomorrowLogs <- getLogsByIndex prx (toIndexName prx "katip-elasticsearch-tests-2016-03-06") -- is on sunday, so uses current date
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
getLogs
  :: ( TestESVersion v
     , Monad (BH v IO)
     , Functor (BH v IO)
     , Show (IndexName v)
     )
  => proxy v
  -> IO [Value]
getLogs prx = getLogsByIndex prx (ixn prx)


-------------------------------------------------------------------------------
getLogsByIndex
  :: ( TestESVersion v
     , Monad (BH v IO)
     , Functor (BH v IO)
     , Show (IndexName v)
     )
  => proxy v
  -> IndexName v
  -> IO [Value]
getLogsByIndex prx i = do
  r <- bh prx $ do
    void (refreshIndex prx i)
    searchByIndex prx i (mkSearch prx Nothing Nothing)
  let actualCode = statusCode (responseStatus r)
  assertBool ("search by " <> show i <> " " <> show actualCode <> " /= 200") (actualCode == 200)
  return $ responseBody r ^.. key "hits" . key "hits" . values


-------------------------------------------------------------------------------
bh :: TestESVersion v => proxy v -> BH v IO a -> IO a
bh prx = withBH prx defaultManagerSettings (svr prx)


-------------------------------------------------------------------------------
withTestLogging
  :: TestESVersion v
  => proxy v
  -> IO Scribe
  -> (IO (Response ByteString) -> KatipT IO b)
  -> IO b
withTestLogging = withTestLogging' id


-------------------------------------------------------------------------------
withTestLogging'
  :: (TestESVersion v)
  => (LogEnv -> LogEnv)
  -> proxy v
  -> IO Scribe
  -> (IO (Response ByteString) -> KatipT IO b)
  -> IO b
withTestLogging' modEnv prx setup f = do
  scr <- setup
  le <- modEnv <$> initLogEnv ns env
  le' <- registerScribe "es" scr defaultScribeSettings le
  let done' = do
        _ <- closeScribes le'
        bh prx (refreshIndex prx (ixn prx))
  runKatipT le' (f done')
  where
    ns = Namespace ["katip-test"]
    env = Environment "test"


-------------------------------------------------------------------------------
svr :: TestESVersion v => proxy v -> Server v
svr prx = toServer prx  "http://localhost:9200"


-------------------------------------------------------------------------------
ixn :: TestESVersion v => proxy v -> IndexName v
ixn prx = toIndexName prx "katip-elasticsearch-tests"


-------------------------------------------------------------------------------
ixs :: TestESVersion v => proxy v -> IndexSettings v
ixs prx = defaultIndexSettings prx
  & indexShards prx .~ toShardCount prx 1
  & indexReplicas prx .~ toReplicaCount prx 1


-------------------------------------------------------------------------------
tn :: TestESVersion v => proxy v -> TemplateName v
tn prx = toTemplateName prx "katip-elasticsearch-tests"


-------------------------------------------------------------------------------
mn :: TestESVersion v => proxy v -> MappingName v
mn prx = toMappingName prx "logs"


-------------------------------------------------------------------------------
dropESSchema
  :: ( TestESVersion v
     , Monad (BH v IO)
     , Functor (BH v IO)
     )
  => proxy v
  -> BH v IO ()
dropESSchema prx = void $ deleteIndex prx (toIndexName prx "katip-elasticsearch-tests*")


-------------------------------------------------------------------------------
dropESSTemplate
  :: ( TestESVersion v
     , Monad (BH v IO)
     , Functor (BH v IO)
     )
  => proxy v
  -> BH v IO ()
dropESSTemplate prx = void $ deleteTemplate prx (tn prx)


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
