{-# LANGUAGE OverloadedStrings #-}

module Handler.Posts where

import Import
import Text.Read
import Data.Text.Conversions
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Dto.PostDTO

import Prelude


getPostsR :: Handler Value
getPostsR = do
    offsetMaybe <- lookupGetParam "offset"
    limitMaybe <- lookupGetParam "limit"
    total <- runDB $ count[PostPublished ==. True]
    case limitMaybe of
        Just limit -> do
            case offsetMaybe of
                Just offset -> do
                    posts <- runDB $ E.select $ E.from $ \post -> do
                        E.where_ (post ^. PostPublished)
                        E.orderBy [E.asc (post E.^. PostId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        E.offset (read (fromText $ offset) :: Int64)
                        return post
                    returnJson (posts, total)
                Nothing -> do
                    posts <- runDB $ E.select $ E.from $ \post -> do
                        E.where_ (post ^. PostPublished)
                        E.orderBy [E.asc (post E.^. PostId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        return post
                    returnJson (posts, total)
        Nothing ->  do
            posts <- runDB $ E.select $ E.from $ \post -> do
                E.where_ (post ^. PostPublished)
                E.orderBy [E.asc (post E.^. PostId)]
                return (post)
            returnJson (posts, total)
    

postPostsR :: Handler Value
postPostsR = do
    postDto <- (requireCheckJsonBody :: Handler CreatePostDto)
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            now <- liftIO getCurrentTime
            let post = Post (title postDto) (slug postDto) (content postDto) (image postDto) (published postDto) now now currentUserId
            insertedPost <- runDB $ insertEntity post
            returnJson insertedPost


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
    liftIO (Prelude.print comments)
    let authorIds = Import.map (\(Entity _ q) -> commentUserId q) comments
    authors <- runDB $ selectList [AuthorUserId <-. authorIds] []
    liftIO (Prelude.print authorIds)
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

getEnablePostR :: PostId -> Handler Value
getEnablePostR postId = do
    oldPost <- runDB $ get404 postId
    let newPostToInsert = oldPost {postPublished = True}
    saved <- runDB $ replace postId newPostToInsert
    returnJson saved

getDisablePostR :: PostId -> Handler Value
getDisablePostR postId = do
    oldPost <- runDB $ get404 postId
    let newPostToInsert = oldPost {postPublished = False}
    saved <- runDB $ replace postId newPostToInsert
    returnJson saved


getPostStatisticR :: Handler Value
getPostStatisticR = do
    publishedPosts <- runDB $ count[PostPublished ==. True]
    unpublishedPosts <- runDB $ count[PostPublished ==. False]
    returnJson (publishedPosts, unpublishedPosts)