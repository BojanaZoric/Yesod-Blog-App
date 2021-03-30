module Handler.Category where

import Import

getCategoryR :: CategoryId -> Handler Value
getCategoryR categoryId = do
    maybeCategory <- runDB $ get categoryId
    case maybeCategory of
        Nothing -> error "There is no tag with that id"
        Just category -> returnJson category

putCategoryR :: CategoryId -> Handler Value
putCategoryR = error "Not implemented"

deleteCategoryR :: CategoryId -> Handler Value
deleteCategoryR categoryId = error "Not yet implemented: deleteCategoryR"
