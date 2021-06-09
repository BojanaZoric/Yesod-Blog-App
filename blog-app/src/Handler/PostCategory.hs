module Handler.PostCategory where

import Import

postPostCategoryR :: Handler Value
postPostCategoryR = do
    postCategory <- (requireCheckJsonBody :: Handler CategoryPost)
    insertedPostCategory <- runDB $ insertEntity postCategory
    returnJson insertedPostCategory
