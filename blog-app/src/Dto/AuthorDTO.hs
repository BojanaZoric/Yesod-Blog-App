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
}

instance FromJSON ProfileDataDto where
    parseJSON (Object v) = 
        ProfileDataDto <$> v .: "username"
                       <*> v .: "email"
                       <*> v .: "firstName"
                       <*> v .: "lastName"
                       <*> v .: "biography"
    parseJSON _ = mzero

instance ToJSON ProfileDataDto where
    toJSON (ProfileDataDto username email firstName lastName biography) = 
        object [ "username" .= username
                ,"email" .= email
                ,"firstName" .= firstName
                ,"lastName"  .= lastName
                ,"biography" .= biography
               ]