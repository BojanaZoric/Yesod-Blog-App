{-# LANGUAGE OverloadedStrings #-}

module Handler.Author where

import Import
import Dto.AuthorDTO
import qualified Text.Read as Read
import Data.Text.Conversions

getAuthorR :: AuthorId -> Handler Value
getAuthorR authorId = do
    maybeAuthor <- runDB $ get404 authorId
    writenPosts <- runDB $ selectList[PostAuthorId ==. (authorUserId maybeAuthor)][]
    returnJson (maybeAuthor, writenPosts)

getProfileR :: Handler Value
getProfileR = do
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            currentUser <- runDB $ get404 currentUserId
            currentAuthor <- runDB $ getBy $ UniqueAuthorUser currentUserId
            case currentAuthor of
                Nothing -> do
                    let profile = ProfileDataDto ( userUsername currentUser) (userEmail currentUser) Nothing Nothing Nothing
                    returnJson profile
                Just (Entity currentAuthor author) -> do
                    let profile = ProfileDataDto ( userUsername currentUser) (userEmail currentUser) (Just $ authorFirstName author) (Just $ authorLastName author) ( Just $ authorBiography author)
                    returnJson profile

getMyPostsR :: Handler Value
getMyPostsR = do
    offsetMaybe <- lookupGetParam "offset"
    limitMaybe <- lookupGetParam "limit"
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            total <- runDB $ count[PostAuthorId ==. currentUserId]
            case limitMaybe of
                Just limit -> do
                    case offsetMaybe of
                        Just offset -> do
                            writenPosts <- runDB $ selectList[PostAuthorId ==. currentUserId][OffsetBy (Read.read (fromText $ offset) :: Int), LimitTo (Read.read (fromText $ limit) :: Int)]
                            returnJson (writenPosts, total)
                        Nothing -> do
                            writenPosts <- runDB $ selectList[PostAuthorId ==. currentUserId][LimitTo (Read.read (fromText $ limit) :: Int)]
                            returnJson (writenPosts, total)
                Nothing -> do
                    writenPosts <- runDB $ selectList[PostAuthorId ==. currentUserId][]
                    returnJson (writenPosts, total)

putAuthorR :: AuthorId -> Handler Html
putAuthorR authorId = error "Not yet implemented: putAuthorR"

deleteAuthorR :: AuthorId -> Handler Value
deleteAuthorR authorId = do
    maybeAuthor <- runDB $ get authorId
    case maybeAuthor of
        Nothing -> error ""
        Just author -> do
            runDB $ delete authorId
            returnJson author