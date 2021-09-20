{-# LANGUAGE OverloadedStrings #-}
module Handler.PostTag where

import Import
import Text.Read
import Data.Text.Conversions
import qualified Database.Esqueleto as E

getPostTagR :: TagId -> Handler Value
getPostTagR tagId = do
    tag <- runDB $ Import.get tagId
    offsetMaybe <- lookupGetParam "offset"
    limitMaybe <- lookupGetParam "limit"
    total <- runDB $ count[PostTagTagId ==. tagId]
    case limitMaybe of
        Just limit -> do
            case offsetMaybe of
                Just offset -> do
                    posts <- runDB 
                        $ E.select
                        $ E.from $ \(post `E.InnerJoin` relation) -> do
                            E.on $ post E.^. PostId E.==. relation E.^. PostTagPostId
                            E.where_ $ relation E.^. PostTagTagId E.==. E.val tagId
                            E.orderBy [E.asc (post E.^. PostId)]
                            E.limit (read (fromText $ limit) :: Int64)
                            E.offset (read (fromText $ offset) :: Int64)
                            return post
                    returnJson(tag, posts, total)
                Nothing -> do
                    posts <- runDB 
                        $ E.select
                        $ E.from $ \(post `E.InnerJoin` relation) -> do
                            E.on $ post E.^. PostId E.==. relation E.^. PostTagPostId
                            E.where_ $ relation E.^. PostTagTagId E.==. E.val tagId
                            E.orderBy [E.asc (post E.^. PostId)]
                            E.limit (read (fromText $ limit) :: Int64)
                            return post
                    returnJson (tag, posts, total)
        Nothing ->  do
            posts <- runDB 
                $ E.select
                $ E.from $ \(post `E.InnerJoin` relation) -> do
                    E.on $ post E.^. PostId E.==. relation E.^. PostTagPostId
                    E.where_ $ relation E.^. PostTagTagId E.==. E.val tagId
                    return post
            returnJson(tag, posts, total)

postPostTagR :: TagId -> Handler Value
postPostTagR tagId = do
    postTag <- (requireCheckJsonBody :: Handler PostTag)
    insertedPostTag <- runDB $ insertEntity postTag
    returnJson insertedPostTag

deletePostTagR :: TagId -> Handler Value
deletePostTagR tagId = error "Not yet implemented: deletePostTagR"


deleteRemoveTagPostR :: TagId -> PostId -> Handler Value
deleteRemoveTagPostR tagId postId = do
    postTag <- runDB $ getBy $ UniquePostTag postId tagId
    runDB $ deleteBy $ UniquePostTag postId tagId
    returnJson postTag