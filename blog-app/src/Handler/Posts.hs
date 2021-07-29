{-# LANGUAGE OverloadedStrings #-}

module Handler.Posts where

import Import
import Text.Read
import Data.Text.Conversions
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Dto.PostDTO


getPostsR :: Handler Value
getPostsR = do
    offsetMaybe <- lookupGetParam "offset"
    limitMaybe <- lookupGetParam "limit"
    total <- runDB $ count[PostTitle !=. ""]
    case limitMaybe of
        Just limit -> do
            case offsetMaybe of
                Just offset -> do
                    posts <- runDB $ E.select $ E.from $ \post -> do
                        E.orderBy [E.asc (post E.^. PostId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        E.offset (read (fromText $ offset) :: Int64)
                        return post
                    returnJson (posts, total)
                Nothing -> do
                    posts <- runDB $ E.select $ E.from $ \post -> do
                        E.orderBy [E.asc (post E.^. PostId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        return post
                    returnJson (posts, total)
        Nothing ->  do
            posts <- runDB $ E.select $ E.from $ \post -> do
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

