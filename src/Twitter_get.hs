{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Twitter_get (
read_all_todos_from_twitter
)

where
import Data.ByteString (ByteString)
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Text as T
import Control.Applicative
import GHC.Generics
import Data.Maybe

import Todo
import Utils
import TwitterParser

read_all_todos_from_twitter :: IO  (Either String [Todo])
read_all_todos_from_twitter = do
  ets <- timeline "Task_Menedger"
  case ets of
   Left err -> return $ Left err
   Right ts  -> return $ Right $ fmap fromJust $  Prelude.filter (isJust) $ Prelude.map convert_from_tweet_to_todo  ts

timeline :: String
         -> IO (Either String [Tweet]) 
          
timeline name = do
  req <- parseRequest $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
  res <- withManager $ \m -> do
           signedreq <- signOAuth myoauth mycred req
           httpLbs signedreq m
  return $ eitherDecode  $ responseBody res


instance FromJSON Tweet
instance ToJSON Tweet


myoauth :: OAuth
myoauth =
  newOAuth { oauthServerName     = "api.twitter.com"
           , oauthConsumerKey    = "YlMYa5hz1QxKoPGaLAdsxlPVp"
           , oauthConsumerSecret = "eiNbUvrnri1ympCEQAdKGfx9jgRfUhfZuiyBEDrb09Z39fUB4e"
             }

mycred :: Credential
mycred = newCredential "811851116518506497-pVmw3kN2c3SkXSz3xnuVifwWWuqEaK3"
                       "p9dZ51jpQyoht2YoKphzCAE8HOjcHGdBfNyFMoznR8QGr"
