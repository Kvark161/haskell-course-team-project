{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module Twitter_post (
post_task) where


import           Control.Monad
import           Data.Aeson
import           GHC.Generics
import           Data.ByteString
import qualified Data.ByteString.Char8   as B
import qualified Data.ByteString.Lazy    as BL
import qualified Network.HTTP.Base       as HTTP
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           Web.Authenticate.OAuth

post_task::String->IO ()
post_task task = tweet config task >> return ()

config::Config
config = Config "YlMYa5hz1QxKoPGaLAdsxlPVp" "eiNbUvrnri1ympCEQAdKGfx9jgRfUhfZuiyBEDrb09Z39fUB4e" "811851116518506497-pVmw3kN2c3SkXSz3xnuVifwWWuqEaK3" "p9dZ51jpQyoht2YoKphzCAE8HOjcHGdBfNyFMoznR8QGr"

data Config = Config {
    apiKey       :: String,
    apiSecret    :: String,
    userKey      :: String,
    userSecret   :: String
  } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

oauthTwitter :: ByteString -> ByteString -> OAuth
oauthTwitter key secret =
  newOAuth { oauthServerName      = "twitter.com/Task_Menedger"
           , oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
           , oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
           , oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize"
           , oauthSignatureMethod = HMACSHA1
           , oauthConsumerKey     = key
           , oauthConsumerSecret  = secret
           , oauthVersion         = OAuth10a
           }
           
signWithConfig :: Config -> Request -> IO Request
signWithConfig Config{..} = signOAuth
  (oauthTwitter (B.pack apiKey) (B.pack apiSecret))
  (newCredential (B.pack userKey) (B.pack userSecret))
  
tweet :: Config -> String -> IO (Response BL.ByteString)
tweet config status = do
  url <- parseRequest $ "https://api.twitter.com/1.1/statuses/update.json?status=" ++ HTTP.urlEncode status
  req <- signWithConfig config url{ method = "POST" }
  manager <- newManager tlsManagerSettings
  httpLbs req manager



