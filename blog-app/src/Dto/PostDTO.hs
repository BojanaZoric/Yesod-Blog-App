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
    , published :: Bool
} deriving Show

instance FromJSON CreatePostDto where
    parseJSON (Object v) = 
        CreatePostDto <$> v .: "title"
                      <*> v .: "slug"
                      <*> v .: "content"
                      <*> v .: "published"
    parseJSON _ = mzero

instance ToJSON CreatePostDto where
    toJSON (CreatePostDto title slug content published) = 
        object [ "title" .= title
                ,"slug" .= slug
                ,"content" .= content
                ,"published" .= published
               ]