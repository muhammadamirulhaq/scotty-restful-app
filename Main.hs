{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Web.Scotty

import Data.Aeson (FromJSON, ToJSON, decode)

import Control.Monad.IO.Class ( liftIO )

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Control.Monad.IO.Class ( liftIO )

import Database.MySQL.Protocol.MySQLValue
import Data.Text
import Data.Int
import qualified Data.ByteString.Lazy as B

import Data.String (fromString)

data Users = Users {
	id :: Int32,
	firstname :: Text,
	lastname :: Text,
	username :: Text,
	pass :: Text
} deriving (Show, Generic)
instance ToJSON Users
instance FromJSON Users

getMySQLInt (MySQLInt32 myint) = myint
getMySQLText (MySQLText mytext) = mytext

run_connection =
  connect $ 
    defaultConnectInfo
      { ciUser = "root"
      , ciPassword = "12345678"
      , ciDatabase = "web01" }


main = do
  conn <- run_connection
  scotty 80 $ do
    get "/api/users" $ do
      (defs, is) <- liftIO $ query_ conn "SELECT * FROM users"
      ms_val <- liftIO $ Streams.toList is
      let ms_json = [Users (getMySQLInt $ i!!0) (getMySQLText $ i!!1) (getMySQLText $ i!!2) (getMySQLText $ i!!3) (getMySQLText $ i!!4) | i <- ms_val]
      liftIO (print ms_json)
      json ms_json
      
    get "/api/users/:id" $ do
	  id <- param "id"
	  
	  setHeader "Content-Type" "application/json"
	  
	  (defs, is) <- liftIO $ query_ conn "SELECT * FROM users"
	  ms_val <- liftIO $ Streams.toList is
	  let ms_json = [Users (getMySQLInt $ i!!0) (getMySQLText $ i!!1) (getMySQLText $ i!!2) (getMySQLText $ i!!3) (getMySQLText $ i!!4) | i <- ms_val, (getMySQLInt $ i!!0) == (id :: Int32)]
	  liftIO (print ms_json)
 	      
	  json ms_json
	  
    post "/api/users" $ do
      user <- jsonData :: ActionM Users
      
      let fname = fromString ( unpack $ firstname user )
          lname = fromString ( unpack $ lastname user )
          uname = fromString ( unpack $ username user )
          pwd = fromString ( unpack $ pass user )
          
      liftIO $ print fname
      
      liftIO $ execute_ conn (Query ("INSERT INTO users (firstname,lastname,username,pass) VALUES ('"
        <> fname <>"','"<> lname <>"','"<> uname <>"','"<> pwd <>"')"))
        
      text "Posted."
    
    put "/api/users/:id" $ do
      id <- param "id"
      user <- jsonData :: ActionM Users
      
      let fname = fromString ( unpack $ firstname user)
          lname = fromString ( unpack $ lastname user)
          uname = fromString ( unpack $ username user)
          pwd = fromString ( unpack $ pass user)
      
      liftIO $ execute_ conn (Query ("UPDATE users SET firstname='"
        <>fname<>"', lastname='"<>lname<>"', username='"<>uname<>"', pass='"<>pwd<>"' WHERE id='"<>id<>"'"))
      
      text "Updated."
      
    delete "/api/users/:id" $ do
      id <- param "id"
      liftIO $ execute_ conn (Query ("DELETE FROM users WHERE id='"<> id <>"'"))
        
      text "Deleted."
      

-- :set -package io-streams -package aeson
-- curl -X POST localhost:3000/users -d "{\"userName\": \"bar\", \"userId\": 10}" -H 'Content-Type:application/json'
