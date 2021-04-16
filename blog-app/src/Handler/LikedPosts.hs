module Handler.LikedPosts where

import Import
import qualified Database.Esqueleto as E

getLikedPostsR :: AuthorId -> Handler Value
getLikedPostsR authorId = do 
    likes <- runDB
        $ E.select
        $ E.from $ \(post `E.InnerJoin` relation) -> do
            E.on $ post E.^. PostId E.==. relation E.^. PostLikePostId
            E.where_ $ relation E.^. PostLikeAuthorId E.==. E.val authorId
            return post
        returnJson likes