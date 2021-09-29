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


deleteSavePostR :: PostId -> Handler Value
deleteSavePostR postId = do
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            postSave <- runDB $ getBy $ PostSaveUnique postId currentUserId
            runDB $ deleteBy $ PostSaveUnique postId currentUserId
            returnJson postSave

        
