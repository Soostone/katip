{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Katip
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Katip"
  [
    testProperty "JSON cycle Item" $ \(i :: Item ()) ->
       prop_json_cycle i
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
deriving instance Eq Loc
deriving instance Eq LogStr
deriving instance Show Loc
deriving instance Show LogStr
deriving instance (Eq a) => Eq (Item a)
deriving instance (Show a) => Show (Item a)


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
