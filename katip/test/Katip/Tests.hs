{-# LANGUAGE CPP                        #-}
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
import           Control.Applicative as A
import           Data.Aeson
import qualified Data.HashMap.Strict       as HM
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
