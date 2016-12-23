{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Web.Scotty
import Control.Monad.Trans
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Maybe

import Storage
import Twitter_get
import Twitter_post
import Todo
import Utils

matchesId :: Int -> Todo -> Bool
matchesId inId todo = todoId todo == inId

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

    get "/delete/by/id/:id" $ do
      inId <- param "id"
      l <- liftIO $ deleteById inId
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
     
    get "/find/description/:description" $ do
      description <- param "description"
      result <- liftIO $ findByDescription description
      json result
            
    get "/accept/:id" $ do
      inId <- param "id"
      liftIO $ acceptTodo inId
      let s = T.pack ((T.unpack "/todo/") ++(show inId))
      redirect $ L.fromStrict s

--Twitter won't post the same 2 tweets
    get "/twitter/readall" $ do
      l <- liftIO $ read_all_todos_from_twitter
      json l

    get "/twitter/post_task/by/title/:title" $ do
      title <- param "title"
      l <- liftIO $ findByTitle title  
      case l of 
        [] -> text  $ L.pack  ("No task with such title, Nothing was posted")            
        smth->do         result <- mapM (liftIO . post_task . show) l
                         json result
                         text  $ L.pack ("Task(s) with title "++(show title)++" was(were) posted on Twitter!")

    get "/twitter/post_task/by/description/:description" $ do
      description <- param "description"
      l <- liftIO $ findByDescription description  
      case l of 
        [] -> text  $ L.pack  ("No task with such description, Nothing was posted")            
        smth->do         result <- mapM (liftIO . post_task . show) l
                         json result
                         text  $ L.pack ("Task(s) with description "++(show description)++" was(were) posted on Twitter!")
      
    get "/twitter/post_task/by/id/:id" $ do
      inId <- param "id"
      l <- liftIO $ getById inId  
      case l of 
        Just smth->do    result <- liftIO $ post_task (show $ fromJust l)
                         json result
                         text  $ L.pack ("Task with id "++(show inId)++" was posted on Twitter!")
        Nothing -> text  $ L.pack  ("No task with such id, Nothing was posted")            
