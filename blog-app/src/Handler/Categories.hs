module Handler.Categories where

import Import

getCategoriesR :: Handler Value
getCategoriesR = do
    categories <- runDB $ selectList [][Asc CategoryName]
    returnJson categories

postCategoriesR :: Handler Value
postCategoriesR = do
    category <- (requireCheckJsonBody :: Handler Category)
    insertedCategory <- runDB $ insertEntity category
    returnJson insertedCategory
