{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens                hiding (mapping, (.=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import qualified Data.HashMap.Strict         as HM
import qualified Data.Map                    as M
import           Data.Monoid
import           Data.Scientific
import qualified Data.Vector                 as V
import           Database.Bloodhound         hiding (key)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Test.QuickCheck.Instances   ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Katip
import           Katip.Scribes.ElasticSearch
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain $ testGroup "katip-elasticsearch"
  [
    withResource setupSearch teardownSearch esTests
  , typeAnnotatedTests
  ]


-------------------------------------------------------------------------------
setupSearch :: IO (Scribe, IO ())
setupSearch = do
    bh recreateESSchema
    mkEsScribe defaultEsScribeCfg { essAnnotateTypes = True } svr ixn mn DebugS V3


-------------------------------------------------------------------------------
teardownSearch :: (Scribe, IO ()) -> IO ()
teardownSearch (_, finalizer) = do
  finalizer
  bh dropESSchema


-------------------------------------------------------------------------------
esTests :: IO (Scribe, IO ()) -> TestTree
esTests setup = testGroup "elasticsearch scribe"
  [
    testCase "it flushes to elasticsearch" $ withTestLogging setup $ \done -> do
       $(logT) (ExampleCtx True) mempty InfoS "A test message"
       liftIO $ do
         done
         logs <- getLogs
         length logs @?= 1
         let l = head logs
         l ^? key "_source" . key "msg" . _String @?= Just "A test message"
         l ^? key "_source" . key "data" . key "whatever::b" . _Bool @?= Just True
  ]


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
    testCase "annotates values on toJSON" $ do
      toJSON (TypeAnnotated exampleValue) @?= annotatedExampleValue
  , testCase "deannotates on parseJSON" $ do
      parseEither parseJSON (toJSON exampleValue) @?= Right exampleValue
  , testProperty "roundtrips the same as raw" $ \(v :: Value) ->
      let res = typeAnnotatedValue <$> (parseEither parseJSON (toJSON (TypeAnnotated v)))
      in res === Right v
  ]


-------------------------------------------------------------------------------
exampleValue :: Value
exampleValue = Array $ V.fromList [Null, Object ob]
  where
    ob = HM.fromList [ ("a bool", Bool False)
                     , ("a long", Number 24)
                     , ("a double", Number 52.3)
                     , ("a string", String "s")
                     , ("a null", Null)
                     , ("a map", Object (HM.singleton "baz" (Bool True)))
                     ]


-------------------------------------------------------------------------------
annotatedExampleValue :: Value
annotatedExampleValue = Array $ V.fromList
  [
    Null
  , Object $ HM.fromList
    [
      ("a map",Object $ HM.fromList [("baz::b", Bool True)])
    , ("a bool::b", Bool False)
    , ("a null::n", Null)
    , ("a string::s", String "s")
    , ("a double::d", Number 52.3)
    , ("a long::l", Number 24.0)
    ]
  ]


-------------------------------------------------------------------------------
getLogs :: IO [Value]
getLogs = do
  r <- bh $ searchByIndex ixn $ mkSearch Nothing Nothing
  statusCode (responseStatus r) @?= 200
  return $ responseBody r ^.. key "hits" . key "hits" . values


-------------------------------------------------------------------------------
bh :: BH IO a -> IO a
bh = withBH defaultManagerSettings svr


-------------------------------------------------------------------------------
withTestLogging
  :: IO (Scribe, IO a) -> (IO Reply -> KatipT IO b) -> IO b
withTestLogging setup f = do
    (scr, done) <- setup
    le <- initLogEnv ns env
    let done' = done >> bh (refreshIndex ixn)
    runKatipT le { _logEnvScribes = M.singleton "es" scr} (f done')
  where
    ns = Namespace ["katip-test"]
    env = Environment "test"


-------------------------------------------------------------------------------
svr :: Server
svr = Server "http://localhost:9200"


-------------------------------------------------------------------------------
ixn :: IndexName
ixn = IndexName "katip-elasticsearch-tests"


-------------------------------------------------------------------------------
ixs :: IndexSettings
ixs = defaultIndexSettings

-------------------------------------------------------------------------------
mn :: MappingName
mn = MappingName "logs"


-------------------------------------------------------------------------------
mapping :: ()
mapping = () -- ehhh


-------------------------------------------------------------------------------
recreateESSchema :: BH IO Reply
recreateESSchema = dropESSchema >> createESSchema


-------------------------------------------------------------------------------
dropESSchema :: BH IO ()
dropESSchema = void $ deleteIndex ixn


-------------------------------------------------------------------------------
createESSchema :: BH IO Reply
createESSchema = do
  createIndex ixs ixn
  putMapping ixn mn mapping


-------------------------------------------------------------------------------
instance Arbitrary Value where
  arbitrary = oneof
    [ Object <$> reduceSize arbitrary
    , Array . V.fromList <$> reduceSize arbitrary
    , String <$> arbitrary
    , Number <$> (scientific <$> arbitrary <*> arbitrary)
    , Bool <$> arbitrary
    , pure Null
    ]


-------------------------------------------------------------------------------
-- | Reduce the size of Arbitrary input for the given generator
reduceSize :: Gen a -> Gen a
reduceSize f = sized $ \ n -> resize (n `div` 2) f
