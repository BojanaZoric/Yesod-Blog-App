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
    total <- runDB $ count[CategoryName !=. ""]
    case limitMaybe of
        Just limit -> do
            case offsetMaybe of
                Just offset -> do
                    categories <- runDB $ E.select $ E.from $ \category -> do
                        E.orderBy [E.asc (category E.^. CategoryId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        E.offset (read (fromText $ offset) :: Int64)
                        return category
                    returnJson (categories, total)
                Nothing -> do
                    categories <- runDB $ E.select $ E.from $ \category -> do
                        E.orderBy [E.asc (category E.^. CategoryId)]
                        E.limit (read (fromText $ limit) :: Int64)
                        return category
                    returnJson (categories, total)
        Nothing ->  do
            categories <- runDB $ E.select $ E.from $ \category -> do
                E.orderBy [E.asc (category E.^. CategoryId)]
                return (category)
            returnJson (categories, total)
    

postCategoriesR :: Handler Value
postCategoriesR = do
    category <- (requireCheckJsonBody :: Handler Category)
    insertedCategory <- runDB $ insert400 category
    returnJson insertedCategory


getCategoryR :: CategoryId -> Handler Value
getCategoryR categoryId = do
    maybeCategory <- runDB $ get404 categoryId
    returnJson maybeCategory

putCategoryR :: CategoryId -> Handler Value
putCategoryR categoryId = do
    newCategory <- (requireCheckJsonBody :: Handler Category)
    oldCategory <- runDB $ get404 categoryId
    let newCategoryToInsert = oldCategory { categoryName = categoryName newCategory} 
    saved <- runDB $ replace categoryId newCategoryToInsert
    returnJson saved

deleteCategoryR :: CategoryId -> Handler Value
deleteCategoryR categoryId = do
    category <- runDB $ get404 categoryId
    runDB $ deleteWhere [CategoryPostCategoryId ==. categoryId]
    runDB $ delete categoryId
    returnJson category
