{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Concurrent
import           Control.Monad.IO.Class
import           CriterionPlus
import           Data.Aeson
import           Data.Monoid
import           Data.Text              (Text)
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.IO
import           System.Posix
-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------

main :: IO ()
main = benchmark $
  handleScribeBench


-------------------------------------------------------------------------------
handleScribeBench :: Benchmark ()
handleScribeBench = standoff "Katip.Scribes.Handle" $ do
  subject "Bytestring Builder" $ do
    pause
    (Scribe push) <- liftIO $ setup bbFormat
    tid <- liftIO myThreadId
    continue
    whnfIO $ push $ exItem tid
  subject "Bytestring Builder + Colored" $ do
    pause
    (Scribe push) <- liftIO $ setup bbFormatColor
    tid <- liftIO myThreadId
    continue
    whnfIO $ push $ exItem tid
  subject "WL + Colored" $ do
    pause
    (Scribe push) <- liftIO $ setup colorFormat
    tid <- liftIO myThreadId
    continue
    whnfIO $ push $ exItem tid


-------------------------------------------------------------------------------
exItem :: ThreadId -> Item ExPayload
exItem tid = Item {
      itemApp = Namespace ["app"]
    , itemEnv = Environment "production"
    , itemSeverity = Warning
    , itemThread = tid
    , itemHost = "example"
    , itemProcess = CPid 123
    , itemPayload = ExPayload
    , itemMessage = "message"
    , itemTime = mkUTCTime 2015 3 14 1 5 9
    , itemNamespace = Namespace ["foo"]
    , itemLoc = Nothing
    }


-------------------------------------------------------------------------------
data ExPayload = ExPayload

instance ToJSON ExPayload where
  toJSON _ = Object mempty

instance LogContext ExPayload where
  payloadKeys _ _ = AllKeys


-------------------------------------------------------------------------------
mkUTCTime :: Integer -> Int -> Int -> DiffTime -> DiffTime -> DiffTime -> UTCTime
mkUTCTime y mt d h mn s = UTCTime day dt
  where
    day = fromGregorian y mt d
    dt = h * 60 * 60 + mn * 60 + s


-------------------------------------------------------------------------------
setup :: (forall a. LogContext a => Verbosity -> Item a -> Builder) -> IO Scribe
setup format = do
  h <- openFile "/dev/null" WriteMode
  mkHandleScribe' format h Debug V0
