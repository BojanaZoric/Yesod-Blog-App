module Handler.Author where

import Import

getAuthorR :: AuthorId -> Handler Value
getAuthorR authorId = do
    maybeAuthor <- runDB $ get404 authorId
    writenPosts <- runDB $ selectList[PostAuthorId ==. authorId][]
    returnJson (maybeAuthor, writenPosts)

putAuthorR :: AuthorId -> Handler Html
putAuthorR authorId = error "Not yet implemented: putAuthorR"

deleteAuthorR :: AuthorId -> Handler Value
deleteAuthorR authorId = do
    maybeAuthor <- runDB $ get authorId
    case maybeAuthor of
        Nothing -> error ""
        Just author -> do
            runDB $ delete authorId
            returnJson author