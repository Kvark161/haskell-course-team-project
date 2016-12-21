{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Time.LocalTime
import Data.Time.Clock

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
    let currentTimeZone = hoursToTimeZone 3
    ct <- getCurrentTime
    return $ utcToLocalTime currentTimeZone ct
