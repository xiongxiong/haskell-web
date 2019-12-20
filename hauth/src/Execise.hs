module Exercise where

import ClassyPrelude
import Data.Aeson

data User = User {userId :: Int, userName :: Text, userHobbies :: [Text]} deriving (Show)

instance ToJSON User where
    toJSON (User uId name hobbies) = object ["id" .= uId, "name" .= name, "hobbies" .= hobbies]

instance FromJSON User where
    parseJSON = withObject "User" $ \v ->
        User 
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "hobbies"
