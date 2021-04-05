module Handler.Post where

import Import

getPostR :: PostId -> Handler Value
getPostR postId = do
    post <- runDB $ get404 postId
    returnJson post

