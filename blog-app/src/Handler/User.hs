
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.User where

import           Data.Aeson                (object)
import Import
import Database.Persist.Types.Password

getUserR :: Handler Html
getUserR = error "Not yet implemented: getUserR"

postUserR :: Handler Value
postUserR = do
    user <- (requireCheckJsonBody:: Handler User)
    insertedUser <- runDB $ insertEntity user
    returnJson insertedUser

data Login = Login {
    loginUsername :: Text,
    loginPassword :: Text
}

instance FromJSON Login where
 parseJSON (Object v) =
    Login <$> v .: "loginUsername"
           <*> v .: "loginPassword"
 parseJSON _ = mzero

instance ToJSON Login where
 toJSON (Login loginUsername loginPassword) =
    object [ "loginUsername"  .= loginUsername
           , "loginPassword"   .= loginPassword
             ]

postUserLoginR :: Handler Value
postUserLoginR = do
    loginData <- (requireCheckJsonBody:: Handler Login)
    maybeUser <- runDB $ getBy $ UniqueUser $ loginUsername loginData
    case maybeUser of
        Just (Entity userId user@User {..}) | validPwd ->
            encodeUser userId user
            where validPwd = verifyPassword (loginPassword loginData) userPassword
        _ -> 
            error "Unauthorized"

data Register = Register {
    registerUsername :: Text,
    registerEmail :: Text,
    registerPassword :: Text,
    firstName :: Text,
    lastName :: Text,
    biography :: Text
}

instance FromJSON Register where
 parseJSON (Object v) =
    Register <$> v .: "registerUsername"
            <*> v .: "registerEmail"
            <*> v .: "registerPassword"
            <*> v .: "firstName"
            <*> v .: "lastName"
            <*> v .: "biography"
 parseJSON _ = mzero

instance ToJSON Register where
 toJSON (Register registerUsername registerEmail registerPassword firstName lastName biography) =
    object [ "registerUsername"  .= registerUsername
           , "registerEmail"  .= registerEmail
           , "registerPassword"   .= registerPassword
           , "firstName"   .= firstName
           , "lastName"   .= lastName
           , "biography"   .= biography
             ]

postUserRegisterR :: Handler Value
postUserRegisterR = do
    registerData <- (requireCheckJsonBody:: Handler Register)
    hashedPwd <- mkPassword (registerPassword registerData)
    let user = User (registerUsername registerData) ( registerEmail registerData) hashedPwd "author"
    insertedUser <- runDB $ insert400 user
    let author = Author (firstName registerData) (lastName registerData) (biography registerData) insertedUser
    insertedAuthor <- runDB $ insert400 author
    returnJson insertedUser

encodeUser :: UserId -> User -> Handler Value
encodeUser userId User {..} = do
  token <- userIdToToken userId
  return $ object
    [ "user" .= object
        [ "email" .= userEmail
        , "username" .= userUsername
        , "token" .= token
        , "role" .= userRole
        --, "bio" .= userBio
        --, "image" .= userImage
        --, "createdAt" .= userCreatedAt
        --, "updatedAt" .= userUpdatedAt
        ]
    ]
    