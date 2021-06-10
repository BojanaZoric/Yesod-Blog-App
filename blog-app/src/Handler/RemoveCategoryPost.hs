module Handler.RemoveCategoryPost where

import Import

deleteRemoveCategoryPostR :: CategoryPostId -> Handler Value
deleteRemoveCategoryPostR categoryPostId = do
    maybePostCategory <- runDB $ get categoryPostId
    case maybePostCategory of
        Nothing -> error "Error"
        Just postCategory -> do
            runDB $ delete categoryPostId
            returnJson postCategory
