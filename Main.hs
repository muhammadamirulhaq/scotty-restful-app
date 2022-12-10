{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON, object, (.=), Value(..), toJSON, parseJSON, (.:))
import Control.Monad.IO.Class (liftIO)
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Text
import Data.Int
import Data.String

data User = User {
  userid :: Int32,
  username :: Text 
} deriving (Show)

instance ToJSON User where
  toJSON (User id uname) = object [ "userid" .= id, "username" .= uname ]
   
instance FromJSON User where
  parseJSON (Object v) =  User <$>
                            v .: "userid" <*>
                            v .: "username"

getMySQLInt (MySQLInt32 myint) = myint
getMySQLText (MySQLText mytext) = mytext

connect_db = connect $ defaultConnectInfo {
	ciUser = "root",
	ciPassword = "12345678",
	ciDatabase = "test"
}

main = do
  conn <- connect_db
  scotty 80 $ do
    get "/api/users" $ do
      setHeader "Content-Type" "application/json"
      (_, is) <- liftIO $ query_ conn "SELECT * FROM users"
      users_mysql <- liftIO $ Streams.toList is
      let users_json = [User (getMySQLInt $ i!!0) (getMySQLText $ i!!1) | i <- users_mysql]
      liftIO . print $ users_json
      json users_json

    get "/api/users/:id" $ do
      id <- param "id"
      setHeader "Content-Type" "application/json"
      (_, is) <- liftIO $ query_ conn "SELECT * FROM users"
      users_mysql <- liftIO $ Streams.toList is
      let users_json = [User (getMySQLInt $ i!!0) (getMySQLText $ i!!1) | i <- users_mysql, (getMySQLInt $ i!!0) == (id :: Int32)]
      liftIO . print $ users_json
      json users_json
      
    post "/api/users" $ do
      user_json <- jsonData :: ActionM User
      
      let id = fromString (show $ userid user_json)
          uname = fromString ( unpack $ username user_json )
      
      liftIO $ execute_ conn (Query ("INSERT INTO users (id,username) VALUES ('"
        <> id <>"','"<> uname <> "')"))
        
      text "Posted."
      
    put "/api/users/:id" $ do
      id <- param "id"
      user_json <- jsonData :: ActionM User
      
      let uname = fromString ( unpack $ username user_json )
      
      liftIO $ execute_ conn (Query ("UPDATE users SET username='"
        <>uname<>"' WHERE id='"<>id<>"'"))
      
      text "Updated."
      
    delete "/api/users/:id" $ do
      id <- param "id"
      liftIO $ execute_ conn (Query ("DELETE FROM users WHERE id='"<> id <>"'"))
        
      text "Deleted."
