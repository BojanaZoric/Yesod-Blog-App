module Handler.Post where

import Import

getPostR :: Handler Value
getPostR = do
    dt <- runDB $ selectList [][Asc PostId]
    returnJson dt

postPostR :: Handler Value
postPostR = do
    post <- (requireCheckJsonBody :: Handler Post)
    insertedPost <- runDB $ insertEntity post
    returnJson insertedPost

