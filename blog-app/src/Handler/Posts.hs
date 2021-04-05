module Handler.Posts where

import Import

getPostsR :: Handler Value
getPostsR = do
    posts <- runDB $ selectList [][Asc PostId]
    returnJson posts

postPostsR :: Handler Value
postPostsR = do
    post <- (requireCheckJsonBody :: Handler Post)
    insertedPost <- runDB $ insertEntity post
    returnJson insertedPost
