{-# LANGUAGE DeriveGeneric #-}

module Todo where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.Time.LocalTime

data Todo = Todo { todoId :: Int, title :: String, desc :: String, add_date :: LocalTime, end_date :: Maybe LocalTime } deriving (Show, Generic)

instance ToJSON Todo
instance FromJSON Todo