module Handler.Authors where

import Import

getAuthorsR :: Handler Value
getAuthorsR = do
    authors <- runDB $ selectList [][Asc AuthorLastName]
    returnJson authors

postAuthorsR :: Handler Value
postAuthorsR = do
    author <- (requireCheckJsonBody :: Handler Author)
    insertedAuthor <- runDB $ insertEntity author
    returnJson insertedAuthor
