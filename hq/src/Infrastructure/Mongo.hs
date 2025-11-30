module Infrastructure.Mongo (MongoEnv (..), MonadMongo (..), MongoM (..), mkMongoEnv, runMongoAction, runMongoM) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Pool (Pool, PoolConfig, defaultPoolConfig, newPool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB (Host (Host), Pipe, PortID (PortNumber), access, close, connect)
import Database.MongoDB.Query (Action, master)

data MongoEnv = MongoEnv
    { mongoPool :: Pool Pipe
    , mongoDBName :: Text
    }

class (Monad m) => MonadMongo m where
    runMongo :: MongoM a -> m a

newtype MongoM a = MongoM {unMongoM :: ReaderT MongoEnv IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader MongoEnv)

mkMongoEnv :: Text -> Int -> Text -> IO MongoEnv
mkMongoEnv host port dbName = do
    let mongoHost = Host (T.unpack host) (PortNumber (fromIntegral port))

    pool <-
        newPool $
            defaultPoolConfig
                (connect mongoHost)
                close
                60 -- ttl
                10 -- max resources
    return
        MongoEnv
            { mongoPool = pool
            , mongoDBName = dbName
            }

runMongoAction :: Action IO r -> MongoM r
runMongoAction action = do
    MongoEnv{mongoPool, mongoDBName} <- ask
    liftIO $ withResource mongoPool $ \pipe ->
        access pipe master mongoDBName action

runMongoM :: MongoEnv -> MongoM a -> IO a
runMongoM env (MongoM r) = runReaderT r env
