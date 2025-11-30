module Serialize.Str (StrTrans (..)) where

import Data.Data (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB

class StrTrans a where
  toStr :: a -> Text
  fromStr :: Text -> Maybe a

instance {-# OVERLAPPABLE #-} (StrTrans a, Eq a, Show a, Typeable a) => Val a where
  val = String . toStr

  cast' (String d) = fromStr d
  cast' _ = Nothing
