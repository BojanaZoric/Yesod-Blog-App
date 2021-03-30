module Handler.Tags where

import Import

getTagsR :: Handler Value
getTagsR = do
    tags <- runDB $ selectList [][Asc TagName]
    returnJson tags

postTagsR :: Handler Value
postTagsR = do
    tag <- (requireCheckJsonBody :: Handler Tag)
    insertedTag <- runDB $ insertEntity tag
    returnJson insertedTag