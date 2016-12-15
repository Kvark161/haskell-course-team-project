{-# LANGUAGE DeriveGeneric #-}

module Todo where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Todo = Todo { todoId :: Int, desc :: String } deriving (Show, Generic)

instance ToJSON Todo
instance FromJSON Todo