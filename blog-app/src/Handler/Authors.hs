{-# LANGUAGE OverloadedStrings #-}

module Handler.Authors where

import Import
import qualified Database.Esqueleto as E
import Text.Read
import Data.Text.Conversions

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
    insertedAuthor <- runDB $ insertEntity author
    returnJson insertedAuthor
