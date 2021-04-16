module Handler.Post where

import Import
import qualified Database.Esqueleto as E

--getPostR :: PostId -> Handler Value
--getPostR postId = do
--    maybePost <- runDB $ get404 postId
 --   comments <- runDB $ selectList [CommentPostId ==. postId][]
 --   returnJson (maybePost, comments)

getPostR :: PostId -> Handler Value
getPostR postId = do
    maybePost <- runDB $ get404 postId
    author <- runDB 
        $ E.select $
        E.from $ \(p, a) -> do
        E.where_ (p E.^. PostAuthorId E.==. a E.^. AuthorId)
        E.where_ (p E.^. PostId E.==. E.val postId)
        return (a)
    comments <- runDB $ selectList [CommentPostId ==. postId][]
    categories <- runDB 
        $ E.select 
        $ E.from $ \(category `E.InnerJoin` mb) -> do
        E.on $ category E.^. CategoryId E.==. mb E.^. CategoryPostCategoryId
        E.where_ $ mb E.^. CategoryPostPostId E.==. E.val postId
        return (category)
    tags <- runDB 
        $ E.select 
        $ E.from $ \(tag `E.InnerJoin` relation) -> do
        E.on $ tag E.^. TagId E.==. relation E.^. PostTagTagId
        E.where_ $ relation E.^. PostTagPostId E.==. E.val postId
        return (tag)
    returnJson (maybePost, author, comments, categories, tags)
