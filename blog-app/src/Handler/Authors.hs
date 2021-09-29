{-# LANGUAGE OverloadedStrings #-}

module Handler.Authors where

import Import
import qualified Database.Esqueleto as E
import Text.Read
import Dto.AuthorDTO
import qualified Text.Read as Read
import Data.Text.Conversions

import Data.Time.Calendar
import Data.Time
import Prelude

import Data.Map (fromListWith, toList)


getAuthorsR :: Handler Value
getAuthorsR = do
    offsetMaybe <- lookupGetParam "offset"
    limitMaybe <- lookupGetParam "limit"
    total <- runDB $ count[AuthorFirstName !=. ""]
    case limitMaybe of
        Just limit -> do
            case offsetMaybe of
                Just offset -> do
                    authors <- runDB $ E.select $ E.from $ \author -> do
                        E.orderBy [E.asc (author E.^. AuthorId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        E.offset (read (fromText $ offset) :: Int64)
                        return author
                    returnJson (authors, total)
                Nothing -> do
                    authors <- runDB $ E.select $ E.from $ \author -> do
                        E.orderBy [E.asc (author E.^. AuthorId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        return author
                    returnJson (authors, total)
        Nothing -> do
            authors <- runDB $ E.select $ E.from $ \author -> do
                E.orderBy [E.asc (author E.^. AuthorId)]
                return (author)
            returnJson (authors, total)

postAuthorsR :: Handler Value
postAuthorsR = do
    author <- (requireCheckJsonBody :: Handler Author)
    insertedAuthor <- runDB $ insert400 author
    returnJson insertedAuthor


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
                    let profile = ProfileDataDto ( userUsername currentUser) (userEmail currentUser) Nothing Nothing Nothing (userEnabled currentUser)
                    returnJson profile
                Just (Entity currentAuthor author) -> do
                    let profile = ProfileDataDto ( userUsername currentUser) (userEmail currentUser) (Just $ authorFirstName author) (Just $ authorLastName author) ( Just $ authorBiography author) (userEnabled currentUser)
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

putAuthorR :: AuthorId -> Handler Value
putAuthorR authorId = do
    newAuthor <- (requireCheckJsonBody :: Handler Author)
    oldAuthor <- runDB $ get404 authorId
    let newAuthorToInsert = oldAuthor {
        authorFirstName = authorFirstName newAuthor, 
        authorLastName = authorLastName newAuthor, 
        authorBiography = authorBiography newAuthor
        }
    saved <- runDB $ replace authorId newAuthorToInsert
    returnJson saved

deleteAuthorR :: AuthorId -> Handler Value
deleteAuthorR authorId = do
    author <- runDB $ get404 authorId
    runDB $ delete authorId
    returnJson author


getAuthorsYearR :: Integer -> Handler Value
getAuthorsYearR year = do
    let startYear = UTCTime (fromGregorian year 1 1) (secondsToDiffTime 0)
    let endYear = UTCTime (fromGregorian year 12 31) (secondsToDiffTime 86400)
    authorsYear <- runDB $ selectList[UserCreated_at >=. startYear,UserCreated_at <=. endYear][Asc UserCreated_at]
    let newList = fmap getMonth authorsYear
    returnJson (frequency newList)


getMonth :: Entity User -> Int
getMonth user = do
    let (_, month, _) = toGregorian (utctDay (userCreated_at (entityVal user)))
    month

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Data.Map.toList (Data.Map.fromListWith (+) [(x, 1) | x <- xs])


getAuthorStatisticR :: Handler Value
getAuthorStatisticR = do
    activeAuthors <- runDB $ count[UserEnabled ==. True]
    inactiveAuthors <- runDB $ count[UserEnabled ==. False]
    returnJson (activeAuthors, inactiveAuthors)

getUserInfoR :: UserId -> Handler Value
getUserInfoR userId = do
    currentUser <- runDB $ get404 userId
    currentAuthor <- runDB $ getBy $ UniqueAuthorUser userId
    case currentAuthor of
        Nothing -> do
            let profile = ProfileDataDto ( userUsername currentUser) (userEmail currentUser) Nothing Nothing Nothing (userEnabled currentUser)
            returnJson profile
        Just (Entity currentAuthor author) -> do
            let profile = ProfileDataDto ( userUsername currentUser) (userEmail currentUser) (Just $ authorFirstName author) (Just $ authorLastName author) ( Just $ authorBiography author) (userEnabled currentUser)
            returnJson profile

getEnableAuthorR :: UserId -> Handler Value
getEnableAuthorR userId = do
    oldAuthor <- runDB $ get404 userId
    let newAuthorToInsert = oldAuthor {userEnabled = True}
    saved <- runDB $ replace userId newAuthorToInsert
    returnJson saved

getDisableAuthorR :: UserId -> Handler Value
getDisableAuthorR userId = do
    oldAuthor <- runDB $ get404 userId
    let newAuthorToInsert = oldAuthor {userEnabled = False}
    saved <- runDB $ replace userId newAuthorToInsert
    returnJson saved
