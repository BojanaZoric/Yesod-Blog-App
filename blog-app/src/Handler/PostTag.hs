module Handler.PostTag where

import Import
import qualified Database.Esqueleto as E

getPostTagR :: TagId -> Handler Value
getPostTagR tagId = do
    tag <- runDB $ get tagId
    posts <- runDB
        $ E.select
        $ E.from $ \(post `E.InnerJoin` relation) -> do
            E.on $ post E.^. PostId E.==. relation E.^. PostTagPostId
            E.where_ $ relation E.^. PostTagTagId E.==. E.val tagId
            return post
    returnJson (tag, posts)

postPostTagR :: TagId -> Handler Value
postPostTagR tagId = do
    postTag <- (requireCheckJsonBody :: Handler PostTag)
    insertedPostTag <- runDB $ insertEntity postTag
    returnJson insertedPostTag

deletePostTagR :: TagId -> Handler Value
deletePostTagR tagId = error "Not yet implemented: deletePostTagR"
