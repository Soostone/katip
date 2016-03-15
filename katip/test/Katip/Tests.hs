{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Katip.Tests
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
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
  ]


-------------------------------------------------------------------------------
prop_json_cycle :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> Property
prop_json_cycle a = eitherDecode (encode a) === Right a


-------------------------------------------------------------------------------
instance Arbitrary a => Arbitrary (Item a) where
    arbitrary = Item
      <$> arbitrary
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
