module Handler.Tag where

import Import

getTagR :: TagId -> Handler Value
getTagR tagId = do
    maybeTag <- runDB $ get tagId
    case maybeTag of
        Nothing -> error "There is no tag with that id"
        Just tag -> returnJson tag

deleteTagR:: TagId -> Handler Value
deleteTagR tagId = do
    maybeTag <- runDB $ get tagId
    case maybeTag of
        Nothing -> error ""
        Just tag -> do
            runDB $ delete tagId
            returnJson tag
