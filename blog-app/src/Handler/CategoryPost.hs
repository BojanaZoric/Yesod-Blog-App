module Handler.CategoryPost where

import Import
import qualified Database.Esqueleto as E

getCategoryPostR :: CategoryId -> Handler Value
getCategoryPostR categoryId = do
    category <- runDB $ get categoryId
    posts <- runDB 
        $ E.select
        $ E.from $ \(post `E.InnerJoin` relation) -> do
            E.on $ post E.^. PostId E.==. relation E.^. CategoryPostPostId
            E.where_ $ relation E.^. CategoryPostCategoryId E.==. E.val categoryId
            return post
    returnJson(category, posts)