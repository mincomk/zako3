module API.Server where

import Control.Monad.IO.Class (MonadIO (..))
import Infrastructure.App (AppM)
import Servant
import Servant.API

type SampleAPI =
    Get '[JSON] String
        :<|> Capture "text" String :> Get '[JSON] String

type API = "sample" :> SampleAPI

sampleServerM :: ServerT SampleAPI AppM
sampleServerM = getString :<|> getStringMulti
  where
    getString = do
        liftIO . print $ "Hello Empty"
        return "Hello"

    getStringMulti t = do
        liftIO . print $ t ++ "Hello"
        return $ t ++ "Hello"
