module Handler.PostsYear where

import Import
import Data.Time.Calendar
import Data.Time

getPostsYearR :: Integer -> Handler Value
getPostsYearR year = do
    let startYear = UTCTime (fromGregorian year 1 1)
    postsYear <- runDB $ selectList [PostCreated_at >= startYear] []
    return postsYear
