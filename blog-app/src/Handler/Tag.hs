module Handler.Tag where

import Import

getTagR :: TagId -> Handler Value
getTagR tagId = do
    maybeTag <- runDB $ get tagId
    case maybeTag of
        Nothing -> error "There is no tag with that id"
        Just tag -> returnJson tag

putTagR :: TagId -> Handler Value
putTagR = error "not"

deleteTagR:: TagId -> Handler Value
deleteTagR tagId = error "Not yet implemented: deleteTagR"
