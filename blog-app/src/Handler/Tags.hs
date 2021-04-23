module Handler.Tags where

import Import

getTagsR :: Handler Value
getTagsR = do
    liftIO (Prelude.print "tag")
    tags <- runDB $ selectList [][Asc TagName]
    returnJson tags

postTagsR :: Handler Value
postTagsR = do
    liftIO (Prelude.print "tag")
    tag <- (requireCheckJsonBody :: Handler Tag)
    insertedTag <- runDB $ insertEntity tag
    returnJson insertedTag