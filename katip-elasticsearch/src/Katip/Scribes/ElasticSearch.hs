module Katip.Scribes.ElasticSearch where


-------------------------------------------------------------------------------
import           Control.Monad
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
    -> Severity
    -> Verbosity
    -> IO Scribe
mkEsScribe server ix mapping sev verb = return $ Scribe $ \ i -> do
  did <- mkDocId
  void $ indexDocument server ix mapping i did


-------------------------------------------------------------------------------
mkDocId :: IO DocId
mkDocId = (DocId . toString) `fmap` randomIO







