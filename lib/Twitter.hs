module Twitter where

import System.Environment

data Client = Client { getApiKey :: String
                     , getSecretKey :: String
                     , getAccessToken :: String
                     , getAccessSecret :: String
                     }
                     deriving (Show)

twitterApiKey = "TWITTER_API_KEY"
twitterSecretKey = "TWITTER_SECRET_KEY"
twitterAccessToken = "TWITTER_ACCESS_TOKEN"
twitterAccessSecret = "TWITTER_ACCESS_SECRET"
geraTwitterUserId = 1198816177704689665

fromEnv :: IO Client
fromEnv = do
  apiKey <- getEnv twitterApiKey
  secretKey <- getEnv twitterSecretKey
  accessToken <- getEnv twitterAccessToken
  accessSecret <- getEnv twitterAccessSecret
  return $ Client apiKey secretKey accessToken accessSecret

sep :: Char -> String -> (String,String)
sep c s =
    let (k,v) = break (==c) s
    in (k,tail v)

splitLine :: String -> String -> [String] -> [String]
splitLine "" line acc = acc ++ [line]
splitLine ('\n':xs) line acc = splitLine xs "" (acc ++ [line])
splitLine (c:xs) line acc = splitLine xs (line ++ [c]) acc

readLine :: FilePath -> IO [String]
readLine path = do
  content <- readFile path
  return $ splitLine content [] []

fromEnvFile :: FilePath -> IO (Maybe Client)
fromEnvFile path = do
  rows <- readLine path
  let kvs = map (sep '=') rows
  return $ do
    apiKey <- lookup twitterApiKey kvs
    secretKey <- lookup twitterSecretKey kvs
    accessToken <- lookup twitterAccessToken kvs
    accessSecret <- lookup twitterAccessSecret kvs
    return $ Client apiKey secretKey accessToken accessSecret

fromDotEnv :: IO Client
fromDotEnv = do
  mc <- fromEnvFile ".env"
  case mc of
    Just c -> return c
    Nothing -> error "Invalid access info"
