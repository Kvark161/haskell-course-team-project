{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Utils (
        getCurrentLocalTime,
        connectAndRun
    ) where

import Data.Time.LocalTime
import Data.Time.Clock
import Data.Pool
import Database.MySQL.Base
import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Char8 as C

data AppConfiguration = AppConfiguration {  userName :: String,
                                            password :: String,
                                            databaseName :: String,
                                            stripeSize :: Int,
                                            connectionLife :: NominalDiffTime,
                                            poolSize :: Int} deriving (Generic)

instance FromJSON AppConfiguration

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
    let currentTimeZone = hoursToTimeZone 3
    ct <- getCurrentTime
    return $ utcToLocalTime currentTimeZone ct

connectAndRun fun = do
    config <- readConfiguration
    pool <- createPool (createNewConnection config) close (stripeSize config) (connectionLife config) (poolSize config)
    fun pool

createNewConnection config = connect defaultConnectInfoMB4 {
                            ciUser = C.pack (userName config),
                        ciPassword = C.pack (password config),
                        ciDatabase = C.pack (databaseName config)}

readConfiguration = do
            d <- readFile "config.json"
            case decode (Char8.pack d) :: Maybe AppConfiguration of
                Just config -> return config
                Nothing -> error "configuration is incorrect"