{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.AuthorDTO where

import Data.Aeson (object)
import Import

data ProfileDataDto = ProfileDataDto {
      username :: Text
    , email :: Text
    , firstName :: Maybe Text
    , lastName :: Maybe Text
    , biography :: Maybe Text
    , enabled :: Bool
}

instance FromJSON ProfileDataDto where
    parseJSON (Object v) = 
        ProfileDataDto <$> v .: "username"
                       <*> v .: "email"
                       <*> v .: "firstName"
                       <*> v .: "lastName"
                       <*> v .: "biography"
                       <*> v .: "enabled"
    parseJSON _ = mzero

instance ToJSON ProfileDataDto where
    toJSON (ProfileDataDto username email firstName lastName biography enabled) = 
        object [ "username" .= username
                ,"email" .= email
                ,"firstName" .= firstName
                ,"lastName"  .= lastName
                ,"biography" .= biography
                ,"enabled" .= enabled
               ]