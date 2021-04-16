module Handler.SavedPosts where

import Import
import qualified Database.Esqueleto as E

getSavedPostsR :: AuthorId -> Handler Value
getSavedPostsR authorId = error "not implemented"
