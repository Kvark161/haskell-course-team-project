{-# LANGUAGE OverloadedStrings #-}

module Utils (
        getCurrentLocalTime,
        connectAndRun
    ) where

import Data.Time.LocalTime
import Data.Time.Clock
import Data.Pool
import Database.MySQL.Base

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
    let currentTimeZone = hoursToTimeZone 3
    ct <- getCurrentTime
    return $ utcToLocalTime currentTimeZone ct

connectAndRun fun = do
  pool <- createPool createNewConnection close 10 60 10
  fun pool

createNewConnection = connect defaultConnectInfoMB4 {
                            ciUser = "root",
                        ciPassword = "mysql",
                        ciDatabase = "todolist"}