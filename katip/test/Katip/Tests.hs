{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Katip.Tests
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative       as A
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map.Strict           as M
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.Lazy.Builder    as B
import           Data.Time
import           Data.Time.Clock.POSIX
import           Language.Haskell.TH
import           System.Posix.Types
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Katip
import           Katip.Core
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Katip"
  [
    testProperty "JSON cycle Item" $ \(i :: Item ()) ->
      prop_json_cycle i
  , testProperty "renderSeverity/textToSeverity cycle" $ \sev ->
      textToSeverity(renderSeverity sev) === Just sev
  , testProperty "processIDToText/textToProcessID cycle" $ \pid ->
      textToProcessID (processIDToText pid) === Just pid
  , testCase "processIDToText is just the number" $ do
      processIDToText 123 @?= "123"
  , logContextsTests
  , closeScribeTests
  , closeScribesTests
  ]


-------------------------------------------------------------------------------
logContextsTests :: TestTree
logContextsTests = testGroup "logContexts"
  [
    testCase "overwrites with the right-hand side (right-bias)" $ do
      let l1 = liftPayload (SimpleLogPayload [("foo", AnyLogPayload ("a" :: Text))])
          l2 = liftPayload (SimpleLogPayload [("foo", AnyLogPayload ("b" :: Text))])
          l3 = liftPayload (SimpleLogPayload [("foo", AnyLogPayload ("c" :: Text))])
          both = l1 <> l2 <> l3
      toObject both @?= HM.singleton "foo" (String "c")
  , testCase "respects payloadKeys for each constituent payload" $ do
      let everything = liftPayload (SimpleLogPayload [("foo", AnyLogPayload ("a" :: Text))])
          conservative = liftPayload (ConservativePayload "always" "rarely")
          both = everything <> conservative
      payloadKeys V2 both @?= SomeKeys ["often_shown", "rarely_shown", "foo"]
      payloadKeys V1 both @?= SomeKeys ["often_shown", "foo"]
  ]


-------------------------------------------------------------------------------
closeScribeTests :: TestTree
closeScribeTests = testGroup "closeScribe"
  [ testCase "removes the specified scribe" $ do
      (scr, finalizerCalled) <- trivialScribe
      le <- registerScribe "trivial" scr defaultScribeSettings =<< initLogEnv "ns" "test"
      le' <- closeScribe "trivial" le
      closed <- atomically (readTVar finalizerCalled)
      assertBool "finalizer called" closed
      assertBool "should not have trivial key in scribes" (not (M.member "trivial" (_logEnvScribes le')))
  , testCase "does nothing for a missing scribe" $ do
      le <- initLogEnv "ns" "test"
      le' <- closeScribe "nah" le
      assertBool "does not affect scribes" (M.null (_logEnvScribes le'))
  , testCase "re-throws finalizer exceptions" $ do
      (scr, finalizerCalled) <- brokenScribe 1
      le <- registerScribe "broken" scr defaultScribeSettings =<< initLogEnv "ns" "test"
      res <- try (closeScribe "broken" le)
      closed <- atomically (readTVar finalizerCalled)
      assertBool "finalizer called" closed
      case res of
        Left (ScribeBroken scribeNo) -> scribeNo @?= 1
        Right _ -> assertFailure "Expected to throw a ScribeBroken but it did not"
  ]


-------------------------------------------------------------------------------
trivialScribe :: IO (Scribe, TVar Bool)
trivialScribe = do
  finalizerCalled <- newTVarIO False
  let finalizer = atomically (writeTVar finalizerCalled True)
  return (Scribe (const (return ())) finalizer, finalizerCalled)


-------------------------------------------------------------------------------
brokenScribe :: Int -> IO (Scribe, TVar Bool)
brokenScribe scribeNum = do
  finalizerCalled <- newTVarIO False
  let finalizer = do
        atomically (writeTVar finalizerCalled True)
        throw (ScribeBroken scribeNum)
  return (Scribe (const (return ())) finalizer, finalizerCalled)


-------------------------------------------------------------------------------
data BrokenScribeError = ScribeBroken Int deriving (Show, Typeable)


instance Exception BrokenScribeError

-------------------------------------------------------------------------------
closeScribesTests :: TestTree
closeScribesTests = testGroup "closeScribes"
  [ testCase "returns a log env with no scribes" $ do
      (scr, finalizerCalled) <- trivialScribe
      le <- registerScribe "trivial" scr defaultScribeSettings =<< initLogEnv "ns" "test"
      le' <- closeScribes le
      closed <- atomically (readTVar finalizerCalled)
      assertBool "finalizer called" closed
      assertBool "remvoes all scribes" (M.null (_logEnvScribes le'))
  , testCase "throws the first exception encountered after closing all scribes" $ do
     (scr1, finalizerCalled1) <- brokenScribe 1
     (scr2, finalizerCalled2) <- brokenScribe 2
     le <- registerScribe "broken2" scr2 defaultScribeSettings =<< registerScribe "broken1" scr1 defaultScribeSettings =<< initLogEnv "ns" "test"
     res <- try (closeScribes le)
     closed1 <- atomically (readTVar finalizerCalled1)
     assertBool "finalizer 1 called" closed1
     closed2 <- atomically (readTVar finalizerCalled2)
     assertBool "finalizer 2 called" closed2
     case res of
       Left (ScribeBroken scribeNo) -> scribeNo @?= 1
       Right _ -> assertFailure "Expected to throw a ScribeBroken but it did not"
  ]


-------------------------------------------------------------------------------
data ConservativePayload = ConservativePayload {
      oftenShown  :: Text
    , rarelyShown :: Text
    }


instance ToJSON ConservativePayload where
  toJSON ConservativePayload {..} = object ["often_shown" .= oftenShown
                                           ,"rarely_shown" .= rarelyShown]


instance ToObject ConservativePayload


instance LogItem ConservativePayload where
  payloadKeys V1 _ = SomeKeys ["often_shown"]
  payloadKeys V0 _ = SomeKeys []
  payloadKeys _ _  = AllKeys

-------------------------------------------------------------------------------
prop_json_cycle :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> Property
prop_json_cycle a = eitherDecode (encode a) === Right a


-------------------------------------------------------------------------------
instance Arbitrary a => Arbitrary (Item a) where
    arbitrary = Item
      A.<$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (getCleanUTCTime <$> arbitrary)
      <*> arbitrary
      <*> arbitrary


-------------------------------------------------------------------------------
newtype CleanUTCTime = CleanUTCTime {
      getCleanUTCTime :: UTCTime
    }


-------------------------------------------------------------------------------
-- Work around time parsing precision issues in aeson
instance Arbitrary CleanUTCTime where
    arbitrary = CleanUTCTime . posixSecondsToUTCTime . fromInteger <$> arbitrary


-------------------------------------------------------------------------------
deriving instance Arbitrary Namespace
deriving instance Arbitrary Environment
deriving instance Arbitrary ThreadIdText
deriving instance Arbitrary CPid
#if !MIN_VERSION_base(4, 8, 0)
deriving instance Eq Loc
#endif
deriving instance Eq LogStr
deriving instance (Eq a) => Eq (Item a)


-------------------------------------------------------------------------------
instance Arbitrary Loc where
    arbitrary = do
      f <- arbitrary
      p <- arbitrary
      m <- arbitrary
      s <- arbitrary
      return $ Loc f p m s s


-------------------------------------------------------------------------------
instance Arbitrary Severity where
    arbitrary = oneof $ map pure [ DebugS
                                 , InfoS
                                 , NoticeS
                                 , WarningS
                                 , ErrorS
                                 , CriticalS
                                 , AlertS
                                 , EmergencyS
                                 ]


-------------------------------------------------------------------------------
instance Arbitrary LogStr where
    arbitrary = LogStr . B.fromText <$> arbitrary
