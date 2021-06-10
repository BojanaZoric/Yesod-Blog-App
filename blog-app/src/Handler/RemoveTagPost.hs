module Handler.RemoveTagPost where

import Import

deleteRemoveTagPostR :: PostTagId -> Handler Value
deleteRemoveTagPostR postTagId = do
    maybePostTag <- runDB $ get postTagId
    case maybePostTag of
        Nothing -> error "Error"
        Just postTag -> do
            runDB $ delete postTagId
            returnJson postTag
