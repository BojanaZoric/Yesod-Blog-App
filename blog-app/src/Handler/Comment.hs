module Handler.Comment where

import Import
import Dto.CommentDTO

postCommentR :: Handler Value
postCommentR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    commentDto <- (requireCheckJsonBody :: Handler CommentDataDto)

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId

    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            now <- liftIO getCurrentTime
            let comment' = Comment (message commentDto) currentUserId now (postId commentDto) (parentComment commentDto)
            insertedComment <- runDB $ insert400 comment'
            returnJson insertedComment


getPostCommentR :: PostId -> Handler Value
getPostCommentR postId = do
    comments <- runDB $ selectList ([PostId ==. postId])[]
    returnJson comments

postPostCommentR :: PostId -> Handler Value
postPostCommentR postId = error ""


deletePostCommentsR :: CommentId -> Handler Value
deletePostCommentsR commentId = do
    comment <- runDB $ get404 commentId
    runDB $ delete commentId
    returnJson comment
