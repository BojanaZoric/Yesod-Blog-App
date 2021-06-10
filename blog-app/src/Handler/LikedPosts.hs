module Handler.LikedPosts where

import Import
import qualified Database.Esqueleto as E

getLikedPostsR :: AuthorId -> Handler Value
getLikedPostsR authorId = error ""