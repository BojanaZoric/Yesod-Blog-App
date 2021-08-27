module Handler.TagPost where

import Import

postTagPostR :: Handler Value
postTagPostR = do 
    postTag <- (requireCheckJsonBody :: Handler PostTag)
    insertedPostTag <- runDB $ insertEntity postTag
    returnJson insertedPostTag

postSavePostR :: PostId -> Handler Value
postSavePostR postId = do
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            let postSave = PostSave postId currentUserId
            saved <- runDB $ insertEntity postSave
            returnJson saved
