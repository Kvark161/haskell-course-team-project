{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Web.Scotty
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Time.LocalTime
import Data.Time.Clock

import Storage
import Todo

matchesId :: Int -> Todo -> Bool
matchesId inId todo = todoId todo == inId

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
    let currentTimeZone = hoursToTimeZone 3
    ct <- getCurrentTime
    return $ utcToLocalTime currentTimeZone ct
            

main = do
  

  scotty 3000 $ do
    get "/" $ do
        redirect "/hello/username"
  
    get "/hello/:name" $ do
        name <- param "name"
        text ("hello " <> name <> "!")

    get "/list" $ do
      l <- liftIO $ getAll
      json l

    get "/todo/:id" $ do
      inId <- param "id"
      l <- liftIO $ getById inId
      json l

    get "/new/:title/:description" $ do
      title <- param "title"
      description <- param "description"
      ct <- liftIO $ getCurrentLocalTime
      let obj = Todo 0 title description ct Nothing
      result <- liftIO $ insertTodo obj
      json result
      
    get "/find/title/:title" $ do
      title <- param "title"
      result <- liftIO $ findByTitle title
      json result
