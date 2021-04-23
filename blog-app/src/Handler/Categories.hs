{-# LANGUAGE OverloadedStrings #-}

module Handler.Categories where

import Import
import Text.Read
import Data.Text.Conversions
import qualified Database.Esqueleto as E


getCategoriesR ::  Handler Value
getCategoriesR = do
    offsetMaybe <- lookupGetParam "offset"
    limitMaybe <- lookupGetParam "limit"
    case limitMaybe of
        Just limit -> do
            case offsetMaybe of
                Just offset -> do
                    categories <- runDB $ E.select $ E.from $ \category -> do
                        E.orderBy [E.asc (category E.^. CategoryId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        E.offset (read (fromText $ offset) :: Int64)
                        return category
                    returnJson categories
                Nothing -> do
                    categories <- runDB $ E.select $ E.from $ \category -> do
                        E.orderBy [E.asc (category E.^. CategoryId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        return category
                    returnJson categories
        Nothing ->  do
            categories <- runDB $ E.select $ E.from $ \category -> do
                E.orderBy [E.asc (category E.^. CategoryId)]
                return (category)
            returnJson (categories)
    

postCategoriesR :: Handler Value
postCategoriesR = do
    category <- (requireCheckJsonBody :: Handler Category)
    insertedCategory <- runDB $ insertEntity category
    returnJson insertedCategory
