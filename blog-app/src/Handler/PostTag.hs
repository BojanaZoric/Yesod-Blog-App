module Handler.PostTag where

import Import

getPostTagR :: TagId -> Handler Value
getPostTagR tagId = do
    dt <- runDB $ get tagId
    returnJson dt

postPostTagR :: TagId -> Handler Value
postPostTagR tagId = do
    --tag <- runDB $ get tagId
    postTag <- (requireCheckJsonBody :: Handler PostTag)
    insertedPostTag <- runDB $ insertEntity postTag
    returnJson insertedPostTag

deletePostTagR :: TagId -> Handler Html
deletePostTagR tagId = error "Not yet implemented: deletePostTagR"
