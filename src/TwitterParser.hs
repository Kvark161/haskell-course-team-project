{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module TwitterParser
(convert_from_tweet_to_todo, Tweet) where

import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Char
import Data.Text as T
import GHC.Generics


import Todo

data Tweet =
  Tweet { text :: !Text , created_at :: !String
          } deriving (Show, Generic)

parse_time_from_tweet = do 
   day_of_week<- token $ many1 (sat (/=' '))
   month<-token  $ many1 (sat (/=' '))
   day<-token integer
   hour<-token integer
   string ":"
   minute<-token integer
   string ":"
   second<-token integer
   time_zone<-token $ many1 (sat (/=' '))
   year<-token  integer
   return month
   case month of
    "Jan" -> return $ LocalTime (fromGregorian (toInteger year) 1  day) (TimeOfDay hour minute (read $ show second))
    "Feb" -> return $ LocalTime (fromGregorian (toInteger year) 2 day) (TimeOfDay hour minute (read $ show second))
    "Mar" -> return $ LocalTime (fromGregorian (toInteger year) 3 day) (TimeOfDay hour minute (read $ show second))
    "Apr" -> return $ LocalTime (fromGregorian (toInteger year) 4 day) (TimeOfDay hour minute (read $ show second))
    "May" -> return $ LocalTime (fromGregorian (toInteger year) 5 day) (TimeOfDay hour minute (read $ show second))
    "Jun" -> return $ LocalTime (fromGregorian (toInteger year) 6 day) (TimeOfDay hour minute (read $ show second))
    "Jul" -> return $ LocalTime (fromGregorian (toInteger year) 7 day) (TimeOfDay hour minute (read $ show second))
    "Aug" -> return $ LocalTime (fromGregorian (toInteger year) 8 day) (TimeOfDay hour minute (read $ show second))
    "Sep" -> return $ LocalTime (fromGregorian (toInteger year) 9 day) (TimeOfDay hour minute (read $ show second))
    "Oct" -> return $ LocalTime (fromGregorian (toInteger year) 10 day) (TimeOfDay hour minute (read $ show second))
    "Nov" -> return $ LocalTime (fromGregorian (toInteger year) 11 day) (TimeOfDay hour minute (read $ show second))
    "Dec" -> return $ LocalTime (fromGregorian (toInteger year) 12 day) (TimeOfDay hour minute (read $ show second))

parse_time_from_localtime = do 
     year <-token integer
     string "-" 
     month <- token integer 
     string "-"  
     day <- token integer 
     hour <-token integer 
     string ":" 
     minute <-token integer 
     string ":"
     second <-token integer 
     return $ LocalTime (fromGregorian (toInteger year) month day) (TimeOfDay hour minute (read $ show second))



convert_from_tweet_to_todo:: Tweet -> Maybe Todo
convert_from_tweet_to_todo tw = parse ((do id'<-integer
                                           string ". "
                                           title <- many1 (sat (/='\n'))
                                           string ("\n")
                                           description <- many1 (sat (/='\n'))
                                           string ("\nStart time: ")
                                           start_date <- many1 (sat (/='\n'))
                                           string ("\nEnd time:  ")
                                           end_date <- many1 (sat (/='\n'))
                                           return $ Just $ Todo id' title description (parse parse_time_from_localtime start_date) (Just $ parse parse_time_from_localtime end_date))
                                        <|>
                                        (do id'<-integer
                                            string ". "
                                            title <- many1 (sat (/='\n'))
                                            string ("\n")
                                            description <- many1 (sat (/='\n'))
                                            string ("\nStart time: ")
                                            start_date <- many1 (sat (/='\n'))
                                            return $ Just $ Todo id' title description (parse parse_time_from_localtime start_date) Nothing)
                                        <|>   
                                        (do id'<-integer
                                            string ". "
                                            title <- many1 (sat (/='\n'))
                                            string ("\n")
                                            description <- many1 (sat (/='\n'))
                                            return $ Just $ Todo id' title description (parse parse_time_from_tweet $ created_at tw) Nothing)  
                                        <|>
                                         (do return Nothing))                                                
                                   (T.unpack $ text tw) 


newtype Parser a = Parser { apply :: String -> [(a, String)] }

parse :: Parser a -> String -> a
parse p = fst . Prelude.head . apply p

instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  p >>= q = Parser (\s ->
               [ (y, s'') | (x, s') <- apply p s,
                            (y, s'') <- apply (q x) s'])
  fail _ = Parser (\s -> [])

instance MonadPlus Parser where
  mzero = Parser (\s -> [])
  p `mplus` q = Parser (\s -> let ps = apply p s in if Prelude.null ps then apply q s else ps)

instance Functor Parser where
    fmap = liftM
 
instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero


addition' = do
  n <- digit
  char '+'
  m <- digit
  return $ n + m

addition = digit >>= rest
  where
    rest m = (liftM (+m) $ char '+' >> digit) <|> return m


natural = Prelude.foldl1 (\m n -> m *10 + n) `liftM` many1 digit


integer :: Parser Int
integer = (*) <$> minus <*> natural
  where
    minus = (char '-' >> return (-1)) <|> return 1


intList = bracket "[" "]" $ sepBy (token integer) (symbol ",")

getc :: Parser Char
getc = Parser f
  where
    f [] = []
    f (c:cs) = [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat pr = do
  c <- getc
  guard $ pr c
  return c

char :: Char -> Parser ()
char x = sat (==x) >> return ()

string :: String -> Parser ()
string = mapM_ char

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = digitToInt `liftM` sat isDigit

lowers :: Parser String
lowers = (:) <$> lower <*> lowers <|> return ""

optional :: a -> Parser a -> Parser a
optional v p = p <|> return v

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

space :: Parser ()
space = many (sat isSpace) >> return ()

sepBy1 p sep = (:) <$> p <*> many (sep >> p)

sepBy p sep = optional [] (sepBy1 p sep)

symbol s = space >> string s

token p = space >> p

bracket op cl p = do
  symbol op
  x <- p
  symbol cl
  return x

