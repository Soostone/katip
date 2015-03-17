module Katip.Scribes.ElasticSearch where


-------------------------------------------------------------------------------
import           Control.Monad
import           Data.UUID
import           Database.Bloodhound
import           Network.HTTP.Client
import           System.Random
-------------------------------------------------------------------------------
import           Katip.Core
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
mkEsScribe
    :: ManagerSettings
    -> Server
    -> IndexName
    -> MappingName
    -> Severity
    -> Verbosity
    -> IO Scribe
mkEsScribe ms server ix mapping sev verb =
  withManager ms $ \mgr -> do
    let env = BHEnv { bhServer = server
                    , bhManager = mgr
                    }
    return $ Scribe $ \ i -> do
      did <- mkDocId
      when (_itemSeverity i >= sev) $
        void $ runBH env $ indexDocument ix mapping (itemJson verb i) did


-------------------------------------------------------------------------------
mkDocId :: IO DocId
mkDocId = (DocId . toString) `fmap` randomIO
