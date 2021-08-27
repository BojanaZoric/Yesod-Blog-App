{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.PostCategoryDTO where

import Data.Aeson (object)
import Import

data PostCategoryDto = PostCategoryDto {
      postId :: PostId
    , categoryId :: CategoryId
} deriving Show

instance FromJSON PostCategoryDto where
    parseJSON (Object v) = 
        PostCategoryDto <$> v .: "postId"
                      <*> v .: "categoryId"
    parseJSON _ = mzero

instance ToJSON PostCategoryDto where
    toJSON (PostCategoryDto postId categoryId) = 
        object [ "postId" .= postId
                ,"categoryId" .= categoryId
               ]