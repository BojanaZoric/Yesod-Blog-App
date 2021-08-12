{-# LANGUAGE OverloadedStrings#-}

module Handler.SavedPosts where

import Import
import qualified Database.Esqueleto as E
import Text.Read
import Data.Text.Conversions

getSavedPostsR :: Handler Value
getSavedPostsR = do
    maybeCurrentUserId <- maybeAuthId
    offsetMaybe <- lookupGetParam "offset"
    limitMaybe <- lookupGetParam "limit"
    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            total <- runDB $ count[PostSaveAuthorId ==. currentUserId]
            case limitMaybe of
                Just limit -> do
                    case offsetMaybe of
                        Just offset -> do
                            posts <- runDB 
                                $ E.select
                                $ E.from $ \(post `E.InnerJoin` relation) -> do
                                    E.on $ post E.^. PostId E.==. relation E.^. PostSavePostId
                                    E.where_ $ relation E.^. PostSaveAuthorId E.==. E.val currentUserId
                                    E.limit (read (fromText $ limit) :: Int64)
                                    E.offset (read (fromText $ offset) :: Int64)
                                    return post
                            returnJson(posts, total)
                        Nothing -> do
                            posts <- runDB 
                                    $ E.select
                                    $ E.from $ \(post `E.InnerJoin` relation) -> do
                                        E.on $ post E.^. PostId E.==. relation E.^. PostSavePostId
                                        E.where_ $ relation E.^. PostSaveAuthorId E.==. E.val currentUserId
                                        E.limit (read (fromText $ limit) :: Int64)
                                        return post
                            returnJson(posts, total)
                Nothing -> do
                    posts <- runDB 
                        $ E.select
                        $ E.from $ \(post `E.InnerJoin` relation) -> do
                            E.on $ post E.^. PostId E.==. relation E.^. PostSavePostId
                            E.where_ $ relation E.^. PostSaveAuthorId E.==. E.val currentUserId
                            return post
                    returnJson(posts, total)
