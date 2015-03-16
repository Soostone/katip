module Katip.Scribes.ElasticSearch
    ( mkEsScribe
    , mkDocId
    , EsQueueSize
    , mkEsQueueSize
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Aeson
import           Data.UUID
import           Database.Bloodhound
import           System.Random
-------------------------------------------------------------------------------
import           Katip.Core
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
mkEsScribe
    :: Server
    -> IndexName
    -> MappingName
    -> EsQueueSize
    -> Severity
    -> Verbosity
    -> IO Scribe
mkEsScribe server ix mapping qs sev verb = do
    q <- newTBQueueIO $ unEsQueueSize qs
    worker <- startWorker server ix mapping q
    link worker

    return $ Scribe $ \ i ->
      when (_itemSeverity i >= sev) $
        void $ atomically $ tryWriteTBQueue q (itemJson verb i)


-------------------------------------------------------------------------------
mkDocId :: IO DocId
mkDocId = (DocId . toString) `fmap` randomIO


-------------------------------------------------------------------------------
newtype EsQueueSize = EsQueueSize {
       unEsQueueSize :: Int
     } deriving (Show, Eq, Ord, Bounded)


mkEsQueueSize :: Int -> Maybe EsQueueSize
mkEsQueueSize n
  | n >= 0    = Just $ EsQueueSize n
  | otherwise = Nothing


-------------------------------------------------------------------------------
startWorker
    :: Server
    -> IndexName
    -> MappingName
    -> TBQueue Value
    -> IO (Async ())
startWorker server ix mapping q = async $ forever $ do
    v <- atomically $ readTBQueue q
    res <- waitCatch =<< async (sendLog v)
    when (isLeft res) $ inCaseOfEmergency
  where
    sendLog v = void $ indexDocument server ix mapping v =<< mkDocId
    --TODO reenqueue? drop? would be nice to log but we don't have that context
    inCaseOfEmergency = return ()


-------------------------------------------------------------------------------
tryWriteTBQueue :: TBQueue a -> a -> STM Bool
tryWriteTBQueue q v = do
    ok <- not <$> isFullTBQueue q
    when ok $ writeTBQueue q v
    return ok


-------------------------------------------------------------------------------
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
