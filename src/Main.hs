{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Web.Scotty
import Control.Monad.Trans
import Control.Monad.IO.Class

import Storage
import Todo

matchesId :: Int -> Todo -> Bool
matchesId inId todo = todoId todo == inId
            

main = do
  conn <- getConnection
  
  scotty 3000 $ do
    get "/" $ do
        redirect "/hello/username"
  
    get "/hello/:name" $ do
        name <- param "name"
        text ("hello " <> name <> "!")

    get "/list" $ do
      l <- liftIO $ getAll conn
      json l

    get "/todo/:id" $ do
      inId <- param "id"
      l <- liftIO $ getById conn inId
      json l
