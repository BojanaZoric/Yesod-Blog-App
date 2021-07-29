module Handler.Author where

import Import
import Dto.AuthorDTO

getAuthorR :: AuthorId -> Handler Value
getAuthorR authorId = do
    maybeAuthor <- runDB $ get404 authorId
    writenPosts <- runDB $ selectList[PostAuthorId ==. (authorUserId maybeAuthor)][]
    returnJson (maybeAuthor, writenPosts)

getProfileR :: Handler Value
getProfileR = do
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            currentUser <- runDB $ get404 currentUserId
            currentAuthor <- runDB $ getBy $ UniqueAuthorUser currentUserId
            case currentAuthor of
                Nothing -> do
                    let profile = ProfileDataDto ( userUsername currentUser) (userEmail currentUser) Nothing Nothing Nothing
                    returnJson profile
                Just (Entity currentAuthor author) -> do
                    let profile = ProfileDataDto ( userUsername currentUser) (userEmail currentUser) (Just $ authorFirstName author) (Just $ authorLastName author) ( Just $ authorBiography author)
                    returnJson profile

getMyPostsR :: Handler Value
getMyPostsR = do
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
        Nothing -> notAuthenticated
        Just currentUserId -> do
            writenPosts <- runDB $ selectList[PostAuthorId ==. currentUserId][]
            returnJson writenPosts

putAuthorR :: AuthorId -> Handler Html
putAuthorR authorId = error "Not yet implemented: putAuthorR"

deleteAuthorR :: AuthorId -> Handler Value
deleteAuthorR authorId = do
    maybeAuthor <- runDB $ get authorId
    case maybeAuthor of
        Nothing -> error ""
        Just author -> do
            runDB $ delete authorId
            returnJson author