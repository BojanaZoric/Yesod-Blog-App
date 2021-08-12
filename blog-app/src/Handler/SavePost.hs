module Handler.SavePost where

import Import

postSavePostR :: PostId -> Handler Value
postSavePostR postId = do
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            let postSave = PostSave postId currentUserId
            saved <- runDB $ insertEntity postSave
            returnJson saved
