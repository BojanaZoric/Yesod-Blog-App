module Handler.TagPost where

import Import

postTagPostR :: Handler Value
postTagPostR = do 
    postTag <- (requireCheckJsonBody :: Handler PostTag)
    insertedPostTag <- runDB $ insertEntity postTag
    returnJson insertedPostTag
