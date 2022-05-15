{-# LANGUAGE TemplateHaskell #-}

module Twitter where

import Data.Aeson
import Data.Aeson.TH
import Data.List
import Data.Map (Map)
import Data.Monoid ((<>))
import qualified Data.Text as Tx
import GHC.Generics
import Network.HTTP.Req
import System.Environment
import Text.Printf

bearerTokenEnvName = "TWITTER_BEARER_TOKEN"

geraTwitterUserId = 1198816177704689665

newtype Client = Client {getBearerToken :: String} deriving (Show)

fromEnv :: IO Client
fromEnv = do
  v <- getEnv bearerTokenEnvName
  return $ Client v

sep :: Char -> String -> (String, String)
sep c s =
  let (k, v) = break (== c) s
   in (k, tail v)

splitLine :: String -> String -> [String] -> [String]
splitLine "" line acc = acc ++ [line]
splitLine ('\n' : xs) line acc = splitLine xs "" (acc ++ [line])
splitLine (c : xs) line acc = splitLine xs (line ++ [c]) acc

readLine :: FilePath -> IO [String]
readLine path = do
  content <- readFile path
  return $ splitLine content [] []

fromEnvFile :: FilePath -> IO (Maybe Client)
fromEnvFile path = do
  rows <- readLine path
  let kvs = map (sep '=') rows
  return $ do
    v <- lookup bearerTokenEnvName kvs
    return $ Client v

fromDotEnv :: IO Client
fromDotEnv = do
  mc <- fromEnvFile ".env"
  case mc of
    Just c -> return c
    Nothing -> error "Invalid access info"

data Datetime = Datetime
  { getYear :: Integer,
    getMonth :: Integer,
    getDay :: Integer,
    getHour :: Integer,
    getMinute :: Integer,
    getSecond :: Integer
  }
  deriving (Show, Eq)

datetimeToFormattedString :: Datetime -> String
datetimeToFormattedString dt =
  intercalate "-" (mapPadZero [getYear, getMonth, getDay])
    ++ "T"
    ++ intercalate ":" (mapPadZero [getHour, getMinute, getSecond])
    ++ "Z"
  where
    mapPadZero :: [Datetime -> Integer] -> [String]
    mapPadZero fs = [printf "%02d" (f dt) | f <- fs]

searchTweetsApiUrl = https "api.twitter.com" /: "2" /: "users" /: Tx.pack (show geraTwitterUserId) /: "tweets"

data SearchCriteria = SearchCriteria
  { getMaxResulsts :: Int,
    getStartTime :: Maybe Datetime,
    getEndTime :: Maybe Datetime
  }
  deriving (Show)

toQueryParam :: (QueryParam p, Monoid p) => SearchCriteria -> p
toQueryParam (SearchCriteria maxResults Nothing Nothing) =
  "max_results" =: show maxResults
toQueryParam (SearchCriteria maxResults (Just startTime) Nothing) =
  "max_results" =: show maxResults
    <> "start_time" =: datetimeToFormattedString startTime

data Tweet = Tweet
  { getId :: String,
    getText :: Tx.Text
  }
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''Tweet)
