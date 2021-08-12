module Handler.Post where

import Import
import qualified Database.Esqueleto as E

import Dto.PostDTO

getPostR :: PostId -> Handler Value
getPostR postId = do
    maybePost <- runDB $ get404 postId
    author <- runDB 
        $ E.select $
        E.from $ \(p, a) -> do
        E.where_ (p E.^. PostAuthorId E.==. a E.^. AuthorUserId)
        E.where_ (p E.^. PostId E.==. E.val postId)
        return (a)
    comments <- runDB $ selectList [CommentPostId ==. postId][] 
   -- questions <- runDB $ selectList [QuestionTitle !=. ""] [LimitTo 10]
    let authorIds = Import.map (\(Entity _ q) -> commentUserId q) comments
    authors <- runDB $ selectList [AuthorUserId <-. authorIds] []
    let commentsAndAuthors = Import.zip comments authors
    categories <- runDB 
        $ E.select 
        $ E.from $ \(category `E.InnerJoin` relation) -> do
        E.on $ category E.^. CategoryId E.==. relation E.^. CategoryPostCategoryId
        E.where_ $ relation E.^. CategoryPostPostId E.==. E.val postId
        return (category)
    tags <- runDB 
        $ E.select 
        $ E.from $ \(tag `E.InnerJoin` relation) -> do
        E.on $ tag E.^. TagId E.==. relation E.^. PostTagTagId
        E.where_ $ relation E.^. PostTagPostId E.==. E.val postId
        return (tag, relation)
    returnJson (maybePost, author, commentsAndAuthors, categories, tags)


putPostR :: PostId -> Handler Value
putPostR postId  = do
    newPost <- (requireJsonBody :: Handler CreatePostDto)
    oldPost <- runDB $ get404 postId
    now <- liftIO getCurrentTime
    let newPostToInsert = oldPost {postTitle = title newPost, postSlug = slug newPost, postContent = content newPost, postImage = image newPost, postPublished = published newPost, postLast_modified = now}
    saved <- runDB $ replace postId newPostToInsert
    returnJson saved