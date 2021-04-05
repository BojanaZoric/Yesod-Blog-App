module Handler.PostComments where

import Import

deletePostCommentsR :: PostId -> CommentId -> Handler Value
deletePostCommentsR postId commentId = do
    maybeComment <- runDB $ get commentId
    case maybeComment of
        Nothing -> error ""
        Just comment -> do
            runDB $ delete commentId
            returnJson comment
