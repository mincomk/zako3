module Infrastructure.App (AppEnv (..), AppM) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT, asks)
import Control.Monad.Reader.Class (MonadReader)
import Infrastructure.Mongo (MonadMongo (runMongo), MongoEnv, runMongoM)

newtype AppEnv = AppEnv
    { appMongoEnv :: MongoEnv
    }

newtype AppM a = AppM {unAppM :: ReaderT AppEnv IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

instance MonadMongo AppM where
    runMongo mm = AppM $ do
        mongoEnv <- asks appMongoEnv
        liftIO $ runMongoM mongoEnv mm
