module Katip.Tests.Format.Time
  ( tests,
  )
where

-------------------------------------------------------------------------------
import Data.Aeson (toJSON)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Time.Locale.Compat as LC
-------------------------------------------------------------------------------
import Katip.Format.Time
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Katip.Format.Time"
    [ testCase "formatAsLogTime" $ do
        assertEqual "time 1" (formatDT t1) (formatAsLogTime t1)
        assertEqual "time 2" (formatDT t2) (formatAsLogTime t2)
        assertEqual "time 3" (formatDT t3) (formatAsLogTime t3)
        assertEqual "time 4" (formatDT t4) (formatAsLogTime t4)
        assertEqual "time 5" (formatDT t5) (formatAsLogTime t5),
      testCase "formatAsIso8601" $ do
        assertEqual "time 1" (formatDTISO t1) (formatAsIso8601 t1)
        assertEqual "time 2" (formatDTISO t2) (formatAsIso8601 t2)
        assertEqual "time 3" (formatDTISO t3) (formatAsIso8601 t3)
        assertEqual "time 4" (formatDTISO t4) (formatAsIso8601 t4)
        assertEqual "time 5" (formatDTISO t5) (formatAsIso8601 t5),
      testCase "formatAsIso8601 Aeson" $ do
        assertEqual "time 1" (toJSON t1) (toJSON $ formatAsIso8601 t1)
        assertEqual "time 2" (toJSON t2) (toJSON $ formatAsIso8601 t2)
        assertEqual "time 3" (toJSON t3) (toJSON $ formatAsIso8601 t3)
        assertEqual "time 4" (toJSON t4) (toJSON $ formatAsIso8601 t4)
        assertEqual "time 5" (toJSON t5) (toJSON $ formatAsIso8601 t5),
      testProperty "LogTime same as Date.Time" $ \(ArbUTCTime t) ->
        prop_format_log t,
      testProperty "ISO 8601 same as Date.Time" $ \(ArbUTCTime t) ->
        prop_format_iso t,
      testProperty "ISO 8601 same as Aeson" $ \(ArbUTCTime t) ->
        prop_format_aeson t
    ]
  where
    t1 = UTCTime (fromGregorian 2016 12 34) 5025.123456789012
    t2 = UTCTime (fromGregorian 2016 1 23) 5025.123
    t3 = UTCTime (fromGregorian 16 12 1) 025
    t4 = UTCTime (fromGregorian 2000 1 1) 0.000000000001
    t5 = UTCTime (fromGregorian 2000 12 31) 0.100000000001

formatDT :: UTCTime -> Text.Text
formatDT = Text.pack . formatTime LC.defaultTimeLocale "%0Y-%m-%d %H:%M:%S"

formatDTISO :: UTCTime -> Text.Text
formatDTISO = Text.pack . formatTime LC.defaultTimeLocale "%0Y-%m-%dT%H:%M:%S%QZ"

prop_format_log :: UTCTime -> Property
prop_format_log a = formatDT a === formatAsLogTime a

prop_format_iso :: UTCTime -> Property
prop_format_iso a = formatDTISO a === formatAsIso8601 a

prop_format_aeson :: UTCTime -> Property
prop_format_aeson a = toJSON a === toJSON (formatAsIso8601 a)

newtype ArbUTCTime = ArbUTCTime UTCTime
  deriving (Show)

instance Arbitrary ArbUTCTime where
  arbitrary =
    fmap
      (ArbUTCTime . posixSecondsToUTCTime . fromInteger)
      arbitrary
