
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.User where

import           Data.Aeson                (object)
import Import

getUserR :: Handler Html
getUserR = error "Not yet implemented: getUserR"

postUserR :: Handler Value
postUserR = do
    user <- (requireCheckJsonBody:: Handler User)
    insertedUser <- runDB $ insertEntity user
    returnJson insertedUser

{-
encodeUser :: UserId -> User -> Handler Value
encodeUser userId User {..} = do
    token <- userIdToToken userId
    return $ object 
        [
            "user" .= object
              [
                  "username" .= userUsername
              ]
        ]
    -}