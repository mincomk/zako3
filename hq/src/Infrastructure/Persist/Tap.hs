module Infrastructure.Persist.Tap where

import Data.Persist.Tap
import Data.Persist.TapTrans
import Data.Text (Text)
import Database.MongoDB
import qualified Database.MongoDB as M
import Env.Persist.Tap (MonadTaps (..))
import Infrastructure.Mongo (MonadMongo (runMongo), MongoM, runMongoAction)
import Serialize.Bson (BsonTrans (fromBson, toBson))
import Serialize.Str (StrTrans (..))

instance Val TapPermissions where
    val Public = Doc ["type" =: ("public" :: Text)]
    val OwnerOnly = Doc ["type" =: ("owner" :: Text)]
    val (Whitelist uids) = Doc ["type" =: ("whitelist" :: Text), "users" =: uids]
    val (Blacklist uids) = Doc ["type" =: ("blacklist" :: Text), "users" =: uids]

    cast' (Doc d) = do
        t <- cast' =<< M.lookup "type" d
        case t :: Text of
            "public" -> Just Public
            "owner" -> Just OwnerOnly
            "whitelist" -> Whitelist <$> (M.lookup "users" d >>= cast')
            "blacklist" -> Blacklist <$> (M.lookup "users" d >>= cast')
            _ -> Nothing
    cast' _ = Nothing

instance BsonTrans Tap where
    toBson tap =
        let roles = toStr <$> tapRoles tap
         in ["id" =: tapId tap, "ownerId" =: tapOwnerId tap, "occupation" =: tapOccupation tap, "roles" =: roles, "permissions" =: tapPermissions tap]

    fromBson bson = do
        id <- g "id"
        ownerId <- g "ownerId"
        occupation <- g "occupation"
        roles <- g "roles" :: Maybe [Text]
        roles <- mapM fromStr roles
        permissions <- g "permissions"

        pure $ Tap{tapId = id, tapOwnerId = ownerId, tapOccupation = occupation, tapRoles = roles, tapPermissions = permissions}
      where
        g key = M.lookup key bson >>= cast'

tapCollection :: Collection
tapCollection = "taps"

instance (Monad m, MonadMongo m) => MonadTaps m where
    createTap (CreateTap tap) = runMongo . runMongoAction $ do
        insert tapCollection $ toBson tap
        return tap

    getTap id = runMongo . runMongoAction $ do
        doc <- findOne (select ["id" =: id] tapCollection)
        return $ fromBson =<< doc

    deleteTap id = runMongo . runMongoAction $ do
        delete (select ["id" =: id] tapCollection)
