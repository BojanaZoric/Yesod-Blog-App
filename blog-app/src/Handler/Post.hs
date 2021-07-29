module Handler.Post where

import Import
import qualified Database.Esqueleto as E

getPostR :: PostId -> Handler Value
getPostR postId = do
    maybePost <- runDB $ get404 postId
    author <- runDB 
        $ E.select $
        E.from $ \(p, a) -> do
        E.where_ (p E.^. PostAuthorId E.==. a E.^. UserId)
        E.where_ (p E.^. PostId E.==. E.val postId)
        return (a)
    comments <- runDB $ selectList [CommentPostId ==. postId][]
    categories <- runDB 
        $ E.select 
        $ E.from $ \(category `E.InnerJoin` relation) -> do
        E.on $ category E.^. CategoryId E.==. relation E.^. CategoryPostCategoryId
        E.where_ $ relation E.^. CategoryPostPostId E.==. E.val postId
        return (category, relation)
    tags <- runDB 
        $ E.select 
        $ E.from $ \(tag `E.InnerJoin` relation) -> do
        E.on $ tag E.^. TagId E.==. relation E.^. PostTagTagId
        E.where_ $ relation E.^. PostTagPostId E.==. E.val postId
        return (tag, relation)
    returnJson (maybePost, author, comments, categories, tags)


putPostR :: PostId -> Handler Value
putPostR postId  = do
    updatedPost <- (requireJsonBody :: Handler Post)
    saved <- runDB $ replace postId updatedPost
    returnJson saved
    --sendResponseStatus status200 ("UPDATED" :: Text)