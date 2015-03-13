{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE NoMonomorphismRestriction #-}


module Main where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text
-------------------------------------------------------------------------------
import           Katip
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------



main :: IO ()
main = runKatipT _ioLogEnv $ do
    logMsg "example" InfoS "Easy to emit from IO directly!"
    logF myContext "example" InfoS "Here's a more stateful item."
    $(logT) myContext "example" InfoS "Here's one with code location."
    runHandler handler


myContext :: MyContext
myContext = MyContext "blah" 3

data MyContext = MyContext {
      foo :: Text
    , bar :: Int
    }


instance ToJSON MyContext where
    toJSON mc = object ["foo" .= foo mc, "bar" .= bar mc]

instance ToObject MyContext

instance LogItem MyContext where
    payloadKeys _ _ = AllKeys

-------------------------------------------------------------------------------
type Handler a = WebT (KatipT IO) a


runHandler :: Handler a -> KatipT IO a
runHandler h = runReaderT (unWebT h) req
  where
    req = Request "GET" "/foo/bar" "example.com"

withDB :: (ConnInfo -> Handler ()) -> Handler ()
withDB f = f $ ConnInfo "1234" "exampledb" ()


handler :: Handler ()
handler = do
  $(logTM) InfoS "got request"
  withDB $ \ci -> do
    let dbctx = DBLog (ciConnId ci)
    let ns = Namespace ["db", ciConnDB ci]
    runLogT (return dbctx) (return ns) $ do
      liftIO $ putStrLn "DB things!"
      $(logTM) InfoS "calling db"

-------------------------------------------------------------------------------
data Request = Request {
      reqMethod  :: Text
    , reqPath    :: Text
    , reqReferer :: Text
    }

instance ToJSON Request where
    toJSON req = object [ "method"  .= reqMethod req
                        , "path"    .= reqPath req
                        , "referer" .= reqReferer req
                        ]

instance ToObject Request

instance LogItem Request where
  payloadKeys V3 _ = AllKeys
  payloadKeys _ _  = SomeKeys ["method", "path"]

newtype WebT m a = WebT {
      unWebT :: ReaderT Request m a
    } deriving ( Monad
               , Functor
               , Applicative
               , MonadReader Request
               , MonadTrans
               , MonadIO
               , Katip
               )

instance Katip m => KatipContext (WebT m) where
    getKatipContext = liftM liftPayload ask
    getKatipNamespace = return $ Namespace ["myApp"]


-------------------------------------------------------------------------------
data ConnInfo = ConnInfo {
      ciConnId :: Text
    , ciConnDB :: Text
    , ciConn   :: ()
    }

data DBLog = DBLog {
      dblConnId :: Text
    }

instance ToJSON DBLog where
    toJSON dbl = object ["connId" .= dblConnId dbl]

instance ToObject DBLog

instance LogItem DBLog where
    payloadKeys _ _ = AllKeys
