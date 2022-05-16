{-# LANGUAGE DataKinds #-}

module Twitter where

import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad
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

data InvalidSearchCriteria
  = StartTimeMustBeLessThanEndTime
  | MaxResultMustBeUnder100
  | MaxResultMustBeOver1

data TwitterError = MalformedSearchCriteria InvalidSearchCriteria

type ThrowsError a = Either TwitterError a

bearerTokenEnvName = "TWITTER_BEARER_TOKEN"

geraTwitterUserId = 1198816177704689665

newtype Client = Client {getBearerToken :: L.ByteString} deriving (Show)

client :: String -> Client
client = Client . L.pack

fromEnv :: IO Client
fromEnv = do
  v <- getEnv bearerTokenEnvName
  return $ client v

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
    return $ client v

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
  deriving (Show, Eq, Ord)

datetimeToFormattedString :: Datetime -> String
datetimeToFormattedString dt =
  intercalate "-" (mapPadZero [getYear, getMonth, getDay])
    ++ "T"
    ++ intercalate ":" (mapPadZero [getHour, getMinute, getSecond])
    ++ "Z"
  where
    mapPadZero :: [Datetime -> Integer] -> [String]
    mapPadZero fs = [printf "%02d" (f dt) | f <- fs]

searchTweetsApiUrl :: Url 'Https
searchTweetsApiUrl = https "api.twitter.com" /: "2" /: "users" /: Tx.pack (show geraTwitterUserId) /: "tweets"

data SearchCriteria = SearchCriteria
  { getMaxResulsts :: Integer,
    getStartTime :: Maybe Datetime,
    getEndTime :: Maybe Datetime
  }
  deriving (Show)

validateSearchCriteria :: SearchCriteria -> ThrowsError SearchCriteria
validateSearchCriteria sc@(SearchCriteria _ (Just startTime) (Just endTime))
  | startTime < endTime = Right sc
  | otherwise = Left (MalformedSearchCriteria StartTimeMustBeLessThanEndTime)
validateSearchCriteria sc@(SearchCriteria maxResults _ _)
  | maxResults < 1 = Left (MalformedSearchCriteria MaxResultMustBeOver1)
  | maxResults > 100 = Left (MalformedSearchCriteria MaxResultMustBeUnder100)
  | otherwise = Right sc

integerQueryParameter :: (QueryParam p, Monoid p) => Tx.Text -> Integer -> p
integerQueryParameter name = (=:) name . show

maxResultsQueryParameter :: (QueryParam p, Monoid p) => Integer -> p
maxResultsQueryParameter = integerQueryParameter "max_results"

datetimeQueryParameter :: (QueryParam p, Monoid p) => Tx.Text -> Datetime -> p
datetimeQueryParameter name = (=:) name . datetimeToFormattedString

startTimeQueryParamteter :: (QueryParam p, Monoid p) => Datetime -> p
startTimeQueryParamteter = datetimeQueryParameter "start_time"

endTimeQueryParameter :: (QueryParam p, Monoid p) => Datetime -> p
endTimeQueryParameter = datetimeQueryParameter "end_time"

toQueryParam :: (QueryParam p, Monoid p) => SearchCriteria -> p
toQueryParam (SearchCriteria maxResults Nothing Nothing) = maxResultsQueryParameter maxResults
toQueryParam (SearchCriteria maxResults (Just startTime) Nothing) = maxResultsQueryParameter maxResults <> startTimeQueryParamteter startTime
toQueryParam (SearchCriteria maxResults Nothing (Just endTime)) = maxResultsQueryParameter maxResults <> endTimeQueryParameter endTime
toQueryParam (SearchCriteria maxResults (Just startTime) (Just endTime)) =
  maxResultsQueryParameter maxResults
    <> startTimeQueryParamteter startTime
    <> endTimeQueryParameter endTime

data Tweet = Tweet
  { getId :: String,
    getText :: Tx.Text
  }
  deriving (Show)

instance FromJSON Tweet where
  parseJSON = withObject "Tweet" $ \v -> Tweet <$> v .: "id" <*> v .: "text"

newtype Tweets = Tweets { getTweets :: [Tweet] } deriving (Show)

instance FromJSON Tweets where
  parseJSON = withObject "Tweets" $ \v -> Tweets <$> v .: "data"

fetchTwitter :: Client -> SearchCriteria -> IO (JsonResponse Tweets)
fetchTwitter c sc = runReq defaultHttpConfig $ do
  req GET searchTweetsApiUrl NoReqBody jsonResponse $
      toQueryParam sc <>
        oAuth2Bearer ( L.toStrict $ getBearerToken c )
