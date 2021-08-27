module Handler.PostsYear where

import Import
import Data.Time.Calendar
import Data.Time
import Prelude

import Data.Map (fromListWith, toList)

import qualified Database.Esqueleto as E

getPostsYearR :: Integer -> Handler Value
getPostsYearR year = do
    let startYear = UTCTime (fromGregorian year 1 1) (secondsToDiffTime 0)
    let endYear = UTCTime (fromGregorian year 12 31) (secondsToDiffTime 86400)
    postsYear <- runDB $ selectList[PostCreated_at >=. startYear,PostCreated_at <=. endYear][Asc PostCreated_at]
    let newList = fmap getMonth postsYear
    returnJson (frequency newList)


getMonth :: Entity Post -> Int
getMonth post = do
    let (_, month, _) = toGregorian (utctDay (postCreated_at (entityVal post)))
    month

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Data.Map.toList (Data.Map.fromListWith (+) [(x, 1) | x <- xs])
