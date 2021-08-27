{-# LANGUAGE OverloadedStrings #-}

module Handler.Tags where

import Import
import Text.Read
import Data.Text.Conversions
import qualified Database.Esqueleto as E

getTagsR :: Handler Value
getTagsR = do
    offsetMaybe <- lookupGetParam "offset"
    limitMaybe <- lookupGetParam "limit"
    case limitMaybe of
        Just limit -> do
            case offsetMaybe of
                Just offset -> do
                    tags <- runDB $ E.select $ E.from $ \tag -> do
                        E.orderBy [E.asc (tag E.^. TagId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        E.offset (read (fromText $ offset) :: Int64)
                        return tag
                    total <- runDB $ count[TagName !=. ""]
                    returnJson (tags, total)
                Nothing -> do
                    tags <- runDB $ E.select $ E.from $ \tag -> do
                        E.orderBy [E.asc (tag E.^. TagId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        return tag
                    total <- runDB $ count[TagName !=. ""]
                    returnJson (tags, total)
        Nothing ->  do
            tags <- runDB $ E.select $ E.from $ \tag -> do
                E.orderBy [E.asc (tag E.^. TagId)]
                return (tag)
            total <- runDB $ count[TagName !=. ""]
            returnJson (tags, total)

postTagsR :: Handler Value
postTagsR = do
    liftIO (Prelude.print "tag")
    tag <- (requireCheckJsonBody :: Handler Tag)
    insertedTag <- runDB $ insertEntity tag
    returnJson insertedTag

getTagR :: TagId -> Handler Value
getTagR tagId = do
    maybeTag <- runDB $ Import.get tagId
    case maybeTag of
        Nothing -> error "There is no tag with that id"
        Just tag -> returnJson tag

deleteTagR:: TagId -> Handler Value
deleteTagR tagId = do
    tag <- runDB $ get404 tagId
    runDB $ delete tagId
    returnJson tag
