module Handler.PostTag where

import Import

getPostTagR :: TagId -> Handler Value
getPostTagR tagId = do
    postTags <- runDB $ selectList [TagId ==. tagId][]
    returnJson postTags

postPostTagR :: TagId -> Handler Value
postPostTagR tagId = do
    --tag <- runDB $ get tagId
    postTag <- (requireCheckJsonBody :: Handler PostTag)
    insertedPostTag <- runDB $ insertEntity postTag
    returnJson insertedPostTag

deletePostTagR :: TagId -> Handler Html
deletePostTagR tagId = error "Not yet implemented: deletePostTagR"
