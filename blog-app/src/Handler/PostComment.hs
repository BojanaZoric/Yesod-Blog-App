module Handler.PostComment where

import Import

getPostCommentR :: PostId -> Handler Value
getPostCommentR postId = do
    comments <- runDB $ selectList ([PostId ==. postId])[]
    returnJson comments

postPostCommentR :: PostId -> Handler Value
postPostCommentR postId = error ""

deletePostCommentR :: PostId -> Handler Html
deletePostCommentR postId = error "Not yet implemented: deletePostCommentR"
