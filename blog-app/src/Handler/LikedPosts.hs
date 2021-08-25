module Handler.LikedPosts where

import Import
import qualified Database.Esqueleto as E

getLikedPostsR :: AuthorId -> Handler Value
getLikedPostsR authorId = error ""

postPostLikeR :: PostId -> Handler Html
postPostLikeR postId = error "Not yet implemented: postPostLikeR"

deletePostLikeR :: PostId -> Handler Html
deletePostLikeR postId = error "Not yet implemented: deletePostLikeR"
