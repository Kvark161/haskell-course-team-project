{-# LANGUAGE DeriveGeneric #-}

module Todo where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.Time.LocalTime

data Todo = Todo { todoId :: Int, title :: String, desc :: String, add_date :: LocalTime, end_date :: Maybe LocalTime } deriving (Generic)

instance ToJSON Todo
instance FromJSON Todo

instance Show Todo where
 show (Todo id title desc add_date (Just end_date))= (show id)++". "++title++"\n"++desc++"\n"++"Start time: "++(show add_date)++"\n"++"End time: "++(show end_date)++"\n"
 show (Todo id title desc add_date Nothing)= (show id)++". "++title++"\n"++desc++"\n"++"Start time: "++(show add_date)++"\n"++"Task hasn't been completed yet"++"\n"
