{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dto.CommentDTO where

import Data.Aeson (object)
import Import

data CommentDataDto = CommentDataDto {
      message :: Text
    , postId :: PostId
    , parentComment :: Maybe CommentId
}

instance FromJSON CommentDataDto where
    parseJSON (Object v) = 
        CommentDataDto <$> v .: "message"
                       <*> v .: "postId"
                       <*> v .: "parentComment"
    parseJSON _ = mzero

instance ToJSON CommentDataDto where
    toJSON (CommentDataDto message postId parentComment) = 
        object [ "message" .= message
                ,"postId" .= postId
                ,"parentComment" .= parentComment
               ]