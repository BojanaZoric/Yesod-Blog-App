module Handler.Post where

import Import

getPostR :: PostId -> Handler Value
getPostR postId = do
    maybePost <- runDB $ get404 postId
    comments <- runDB $ selectList [CommentPostId ==. postId][]
    returnJson (maybePost, comments)

