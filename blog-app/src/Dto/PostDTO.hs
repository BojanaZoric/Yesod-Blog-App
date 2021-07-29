{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.PostDTO where

import Data.Aeson (object)
import Import

data CreatePostDto = CreatePostDto {
      title :: Text
    , slug :: Text
    , content :: Text
    , image :: Text
    , published :: Bool
}

instance FromJSON CreatePostDto where
    parseJSON (Object v) = 
        CreatePostDto <$> v .: "title"
                      <*> v .: "slug"
                      <*> v .: "content"
                      <*> v .: "image"
                      <*> v .: "published"
    parseJSON _ = mzero

instance ToJSON CreatePostDto where
    toJSON (CreatePostDto title slug content image published) = 
        object [ "title" .= title
                ,"slug" .= slug
                ,"content" .= content
                ,"image"  .= image
                ,"published" .= published
               ]