module Handler.Authors where

import Import

getAuthorsR :: Handler Value
getAuthorsR = do
    authors <- runDB $ selectList [][Asc AuthorLastName]
    returnJson authors

postAuthorsR :: Handler Html
postAuthorsR = error "Not yet implemented: postAuthorsR"
