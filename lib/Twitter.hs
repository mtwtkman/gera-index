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

data Date = Date
  { getYear :: Integer,
    getMonth :: Integer,
    getDay :: Integer
  }

dateToFormattedString :: Date -> String
dateToFormattedString d =
  intercalate "-" (map (printf "%02d") [getYear d, getMonth d, getDay d])

searchTweetsApiUrl = https "api.twitter.com" /: "2" /: "users" /: Tx.pack (show geraTwitterUserId) /: "tweets"

data Tweet = Tweet
  { getId :: String,
    getText :: Tx.Text
  }
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''Tweet)
