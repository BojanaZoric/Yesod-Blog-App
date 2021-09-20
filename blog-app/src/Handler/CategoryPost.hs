{-# LANGUAGE OverloadedStrings #-}
module Handler.CategoryPost where

import Import
import Text.Read
import Data.Text.Conversions
import qualified Database.Esqueleto as E
import Prelude
import Dto.PostCategoryDTO

getCategoryPostR :: CategoryId -> Handler Value
getCategoryPostR categoryId = do
    category <- runDB $ Import.get categoryId
    offsetMaybe <- lookupGetParam "offset"
    limitMaybe <- lookupGetParam "limit"
    total <- runDB $ count[CategoryPostCategoryId !=. categoryId]
    case limitMaybe of
        Just limit -> do
            case offsetMaybe of
                Just offset -> do
                    posts <- runDB 
                        $ E.select
                        $ E.from $ \(post `E.InnerJoin` relation) -> do
                            E.on $ post E.^. PostId E.==. relation E.^. CategoryPostPostId
                            E.where_ $ relation E.^. CategoryPostCategoryId E.==. E.val categoryId
                            E.orderBy [E.asc (post E.^. PostId)]
                            E.limit (read (fromText $ limit) :: Int64)
                            E.offset (read (fromText $ offset) :: Int64)
                            return post
                    returnJson(category, posts, total)
                Nothing -> do
                    posts <- runDB 
                        $ E.select
                        $ E.from $ \(post `E.InnerJoin` relation) -> do
                            E.on $ post E.^. PostId E.==. relation E.^. CategoryPostPostId
                            E.where_ $ relation E.^. CategoryPostCategoryId E.==. E.val categoryId
                            E.orderBy [E.asc (post E.^. PostId)]
                            E.limit (read (fromText $ limit) :: Int64)
                            return post
                    returnJson (category, posts, total)
        Nothing ->  do
            posts <- runDB 
                $ E.select
                $ E.from $ \(post `E.InnerJoin` relation) -> do
                    E.on $ post E.^. PostId E.==. relation E.^. CategoryPostPostId
                    E.where_ $ relation E.^. CategoryPostCategoryId E.==. E.val categoryId
                    return post
            returnJson(category, posts, total)

postPostCategoryR :: Handler Value
postPostCategoryR = do
    postCategory <- (requireCheckJsonBody :: Handler PostCategoryDto)
    liftIO (Prelude.print postCategory)
    let entity = CategoryPost (postId postCategory) (categoryId postCategory)
    insertedPostCategory <- runDB $ insert400 entity
    returnJson insertedPostCategory


deleteRemoveCategoryPostR :: CategoryId -> PostId-> Handler Value
deleteRemoveCategoryPostR categoryId postId = do
    postCategory <- runDB $ getBy $ PostCategoryId postId categoryId
    runDB $ deleteBy $ PostCategoryId postId categoryId
    returnJson postCategory
