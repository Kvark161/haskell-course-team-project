{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty

data Todo = Todo { todoId :: Int, desc :: String } deriving (Show, Generic)
instance ToJSON Todo
instance FromJSON Todo

todo1 :: Todo
todo1 = Todo { todoId = 1, desc = "description 1" }

todo2 :: Todo
todo2 = Todo { todoId = 2, desc = "description 2" }

todos :: [Todo]
todos = [todo1, todo2]

matchesId :: Int -> Todo -> Bool
matchesId inId todo = todoId todo == inId

main = do
  scotty 3000 $ do
    get "/hello/:name" $ do
        name <- param "name"
        text ("hello " <> name <> "!")

    get "/list" $ do
      json todos

    get "/todo/:id" $ do
      inId <- param "id"
      json (filter (matchesId inId) todos)
