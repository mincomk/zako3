module Serialize.Bson (BsonTrans (..)) where

import Database.MongoDB

class BsonTrans a where
    toBson :: a -> Document
    fromBson :: Document -> Maybe a
