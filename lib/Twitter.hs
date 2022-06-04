module Twitter where

import Control.Monad
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Vector as V
import GHC.Generics
import Network.HTTP.Req
import System.Environment
import Text.Printf

data InvalidSearchCriteria
  = StartTimeMustBeLessThanEndTime
  | MaxResultMustBeUnder100
  | MaxResultMustBeOver5

newtype TwitterError = MalformedSearchCriteria InvalidSearchCriteria

type ThrowsError a = Either TwitterError a

bearerTokenEnvName = "TWITTER_BEARER_TOKEN"

geraTwitterUserId = 1198816177704689665

geraHost = "radio.gera.fan"

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
    getDay :: Integer
  }
  deriving (Show, Eq, Ord)

datetimeToFormattedString :: Datetime -> String
datetimeToFormattedString dt =
  intercalate "-" (mapPadZero [getYear, getMonth, getDay])
    ++ "T00:00:00Z"
  where
    mapPadZero :: [Datetime -> Integer] -> [String]
    mapPadZero fs = [printf "%02d" (f dt) | f <- fs]

searchTweetsApiUrl :: Url 'Https
searchTweetsApiUrl = https "api.twitter.com" /: "2" /: "users" /: T.toStrict (T.pack (show geraTwitterUserId)) /: "tweets"

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
  | maxResults < 5 = Left (MalformedSearchCriteria MaxResultMustBeOver5)
  | maxResults > 100 = Left (MalformedSearchCriteria MaxResultMustBeUnder100)
  | otherwise = Right sc

textQueryParameterBuilder :: (QueryParam p, Monoid p) => T.Text -> T.Text -> p
textQueryParameterBuilder = (=:) . T.toStrict

integerQueryParameter :: (QueryParam p, Monoid p) => T.Text -> Integer -> p
integerQueryParameter name = (=:) (T.toStrict name) . show

maxResultsQueryParameter :: (QueryParam p, Monoid p) => Integer -> p
maxResultsQueryParameter = integerQueryParameter "max_results"

datetimeQueryParameter :: (QueryParam p, Monoid p) => T.Text -> Datetime -> p
datetimeQueryParameter name = (=:) (T.toStrict name) . datetimeToFormattedString

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

buildQueryParameter :: (QueryParam p, Monoid p) => SearchCriteria -> p
buildQueryParameter sc =
  toQueryParam sc
    <> textQueryParameterBuilder "tweet.fields" "entities"
    <> textQueryParameterBuilder "exclude" "retweets,replies"

type HashTags = [T.Text]

type TweetId = String

data Tweet = Tweet
  { getId :: TweetId,
    getHashTags :: HashTags,
    getGeraLink :: Maybe L.ByteString
  }
  deriving (Show, Eq, Generic)

instance FromJSON Tweet where
  parseJSON = withObject "Tweet" $
    \v -> do
      id' <- v .: "id"
      entities <- v .: "entities"
      hashtagArray <- withObject "HashtagArray" (.: "hashtags") entities
      hashtags <- withArray "Hashtags" (mapM (withObject "Tag" (.: "tag")) . V.toList) hashtagArray
      urls <- withObject "Urls" (.: "urls") entities
      expandedUrls <- withArray "ExpandedUrls" (mapM (withObject "ExpandedUrl" (.: "expanded_url")) . V.toList) urls
      let geraUrl = case filter (T.isInfixOf geraHost) expandedUrls of
            [] -> mempty
            x : _ -> return $ T.encodeUtf8 x
      return $ Tweet id' hashtags geraUrl

instance ToJSON Tweet where
  toJSON t =
    object
      [ "id" .= getId t,
        "tags" .= getHashTags t,
        "gera_url" .= maybe "" show (getGeraLink t)
      ]

parseTweetArray :: Array -> Parser [Tweet]
parseTweetArray tweets =
  mapM (parseJSON :: Value -> Parser Tweet) (V.toList tweets)

newtype Tweets = Tweets {getTweets :: [Tweet]} deriving (Show, Eq, Generic)

instance FromJSON Tweets where
  parseJSON = withObject "Data" $ \o -> do
    tweetArray <- o .: "data"
    tweets <- withArray "Tweets" parseTweetArray tweetArray
    return $ Tweets (filter (isJust . getGeraLink) tweets)

instance ToJSON Tweets

fetchTwitter :: Client -> SearchCriteria -> IO (JsonResponse Tweets)
fetchTwitter c sc = runReq defaultHttpConfig $ do
  req GET searchTweetsApiUrl NoReqBody jsonResponse $
    buildQueryParameter sc
      <> oAuth2Bearer (L.toStrict $ getBearerToken c)

aggregate :: Client -> SearchCriteria -> IO Tweets
aggregate c sc = do
  resp <- fetchTwitter c sc
  let tweets = responseBody resp
  return tweets
